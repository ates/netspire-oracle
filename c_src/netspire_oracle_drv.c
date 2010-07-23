#include <oci.h>
#include <stdio.h>
#include <string.h>
#include "erl_nif.h"

/* Structures and variables for internal usage */
static short is_connected = 0;

struct oci_error {
    sb4 code;
    text message[512];
};

struct columns
{
    ERL_NIF_TERM column_name;
    ub2 dtype;
    ub2 dsize;
};

static ERL_NIF_TERM atom_true;
static ERL_NIF_TERM atom_false;
static ERL_NIF_TERM atom_error;
static ERL_NIF_TERM atom_undefined;
static ERL_NIF_TERM atom_ok;

/* Oracle handlers */
static OCIEnv *envhp;
static OCIError *errhp;
static OCISvcCtx *svchp = (OCISvcCtx *) 0;

/* NIF interface declarations */
static int load(ErlNifEnv* env, void** priv_data, ERL_NIF_TERM load_info);
static int reload(ErlNifEnv* env, void** priv_data, ERL_NIF_TERM load_info);
static int upgrade(ErlNifEnv* env, void** priv_data, void** old_priv_data, ERL_NIF_TERM load_info);
static void unload(ErlNifEnv* env, void* priv_data);

/* The NIFs */
static ERL_NIF_TERM ora_connect(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM ora_disconnect(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM ora_query(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM check_connection(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);

/* Internal functions */
static int create_connection(CONST OraText *username,
                                  CONST OraText *password,
                                  CONST OraText *database);
static int destroy_connection(void);
static void cleanups(void);

static ERL_NIF_TERM check_error(ErlNifEnv *env, sb4 status);
static ERL_NIF_TERM make_error_tuple(ErlNifEnv *env, struct oci_error *err);

/* Exports */
static ErlNifFunc nif_funcs[] = {
    {"connect", 3, ora_connect},
    {"disconnect", 0, ora_disconnect},
    {"equery", 1, ora_query},
    {"is_connected", 0, check_connection}
};

ERL_NIF_INIT(netspire_oracle_drv, nif_funcs, load, reload, upgrade, unload)

static int load(ErlNifEnv *env, void **priv_data, ERL_NIF_TERM load_info)
{
    /* Initialize oracle handlers */
    OCIEnvCreate((OCIEnv **) &envhp, (ub4) OCI_DEFAULT,
                  (dvoid *) 0, (dvoid * (*)(dvoid *,size_t)) 0,
                  (dvoid * (*)(dvoid *, dvoid *, size_t)) 0,
                  (void (*)(dvoid *, dvoid *)) 0, (size_t) 0, (dvoid **) 0);

    OCIHandleAlloc((dvoid *) envhp, (dvoid **) &errhp, OCI_HTYPE_ERROR,
            (size_t) 0, (dvoid **) 0);
    OCIHandleAlloc((dvoid *) envhp, (dvoid **) &svchp, OCI_HTYPE_SVCCTX,
            (size_t) 0, (dvoid **) 0);

    /* Make default atoms */
    atom_true = enif_make_atom(env, "true");
    atom_false = enif_make_atom(env, "false");
    atom_error = enif_make_atom(env, "error");
    atom_undefined = enif_make_atom(env, "undefined");
    atom_ok = enif_make_atom(env, "ok");

    *priv_data = NULL;
    return 0;
}

static int reload(ErlNifEnv *env, void **priv_data, ERL_NIF_TERM load_info)
{
    return 0;
}

static int upgrade(ErlNifEnv* env, void **priv_data, void **old_priv_data, ERL_NIF_TERM load_info)
{
    return 0;
}

static void unload(ErlNifEnv* env, void *priv_data)
{
    destroy_connection();
    cleanups();
}

static ERL_NIF_TERM
ora_connect(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    ErlNifBinary bin_user, bin_pass, bin_conn;
    sb4 lstat = 0;
    ERL_NIF_TERM ret;
    char *user, *pass, *conn;
    
    if (!enif_inspect_iolist_as_binary(env, argv[0], &bin_user) || 
        !enif_inspect_iolist_as_binary(env, argv[1], &bin_pass) ||
        !enif_inspect_iolist_as_binary(env, argv[2], &bin_conn)) {
        return enif_make_badarg(env);
    }

    if (is_connected)
        return enif_make_tuple2(env, atom_error, enif_make_atom(env, "already_connected"));
    
    user = (char *)malloc(sizeof(char *) * bin_user.size + 1);
    pass = (char *)malloc(sizeof(char *) * bin_pass.size + 1);
    conn = (char *)malloc(sizeof(char *) * bin_conn.size + 1);
    
    enif_get_string(env, argv[0], user, bin_user.size + 1, ERL_NIF_LATIN1);
    enif_get_string(env, argv[1], pass, bin_pass.size + 1, ERL_NIF_LATIN1),
    enif_get_string(env, argv[2], conn, bin_conn.size + 1, ERL_NIF_LATIN1),

    lstat = create_connection(user, pass, conn);

    free((char *)user);
    free((char *)pass);
    free((char *)conn);

    if (lstat) {
        ret = check_error(env, lstat);
        cleanups();
    }else {
        is_connected = 1;
        ret = atom_ok;
    }

    return ret;
}

static ERL_NIF_TERM
ora_query(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{

    ErlNifBinary bin_query;
    ERL_NIF_TERM ret;
    ERL_NIF_TERM *columns = NULL;
    OCIStmt *stmthp = (OCIStmt *)0;
    
    OCIParam *mypard = (OCIParam *)0;
    ub2 dtype, dsize;
    text *col_name = 0;
    ub4 counter = 1, col_name_len, char_semantics;

    struct columns **cols = NULL;

    sb4 lstat = 0, parm_status;

    OCIDefine *define = NULL;
    char *data[100]; /* XXX: NEED TO FIX */

    int i, rownum = 0;
    ERL_NIF_TERM *row = NULL, *rows = NULL;

    if (!enif_inspect_iolist_as_binary(env, argv[0], &bin_query))
        return enif_make_badarg(env);

    char *query = (char *)malloc(sizeof(char *) * bin_query.size + 1);

    enif_get_string(env, argv[0], query, bin_query.size + 1, ERL_NIF_LATIN1);

    query[bin_query.size] = '\0';

    lstat = OCIHandleAlloc(envhp, (dvoid **)&stmthp,
            OCI_HTYPE_STMT, (size_t) 0, (dvoid **) 0);

    if (lstat) return check_error(env, lstat);

    lstat = OCIStmtPrepare(stmthp, errhp, (OraText *)query,
            (ub4)strlen((const signed char *)query), OCI_NTV_SYNTAX,
            OCI_DEFAULT);

    if (lstat) return check_error(env, lstat);
    
    free((char *)query);

    lstat = OCIStmtExecute(svchp, stmthp, errhp, 0, 0,
                (OCISnapshot *) 0, (OCISnapshot *) 0, OCI_DEFAULT);

    if (lstat) return check_error(env, lstat);

    parm_status = OCIParamGet((dvoid *)stmthp, OCI_HTYPE_STMT, errhp,
            (dvoid **)&mypard, (ub4) counter);

    /* retrieve columns information */
    while (parm_status == OCI_SUCCESS)
    {
        cols = (struct columns **)realloc(cols, sizeof(cols) * counter - 1);
        cols[counter - 1] = (struct columns *)malloc(sizeof(struct columns));

        /* retrieve column name length */
        lstat = OCIAttrGet((dvoid *) mypard, (ub4) OCI_DTYPE_PARAM,
                (dvoid **) &col_name, (ub4 *) &col_name_len, (ub4) OCI_ATTR_NAME,
                (OCIError *) errhp);
        
        if (lstat) return check_error(env, lstat);
        
        /* retrieve column data type */
        lstat = OCIAttrGet((dvoid *) mypard, (ub4) OCI_DTYPE_PARAM,
                (dvoid *) &dtype, (ub4 *) 0, (ub4) OCI_ATTR_DATA_TYPE,
                (OCIError *) errhp);

        if (lstat) return check_error(env, lstat);

        /* Retrieve the length semantics for the column */
        char_semantics = 0;
        
        lstat = OCIAttrGet((dvoid*) mypard, (ub4) OCI_DTYPE_PARAM,
                (dvoid*) &char_semantics,(ub4 *) 0, (ub4) OCI_ATTR_CHAR_USED,
                (OCIError *) errhp);

        if (lstat) check_error(env, lstat);
        if (char_semantics) { /* if column type like varchar */
            /* Retrieve the column width in characters */
            lstat = OCIAttrGet((dvoid*) mypard, (ub4) OCI_DTYPE_PARAM,
                    (dvoid*) &dsize, (ub4 *) 0, (ub4) OCI_ATTR_CHAR_SIZE,
                    (OCIError *) errhp);

            if (lstat) check_error(env, lstat);
        }else {
            /* Retrieve the column width in bytes */
            lstat = OCIAttrGet((dvoid*) mypard, (ub4) OCI_DTYPE_PARAM,
                    (dvoid*) &dsize,(ub4 *) 0, (ub4) OCI_ATTR_DATA_SIZE,
                    (OCIError *) errhp);

            if (lstat) check_error(env, lstat);
        }
        cols[counter - 1]->column_name = enif_make_string_len(env, col_name, col_name_len, ERL_NIF_LATIN1);
        cols[counter - 1]->dtype = dtype;
        cols[counter - 1]->dsize = dsize;

        data[counter - 1] = (dvoid *)malloc(dsize);
        memset(data[counter - 1], 0, dsize);

        /* Ugly hack, it should be fixed */
        lstat = OCIDefineByPos(stmthp, &define, errhp, counter, data[counter - 1],
                dsize, SQLT_STR, 0, 0, 0, OCI_DEFAULT);
        
        if (lstat) return check_error(env, lstat);

        counter++;
        parm_status = OCIParamGet((dvoid *)stmthp, OCI_HTYPE_STMT, errhp,
                          (dvoid **)&mypard, (ub4) counter);
    }
    
    lstat = OCIStmtFetch(stmthp, errhp, 1, OCI_FETCH_NEXT, OCI_DEFAULT);

    while(lstat != OCI_NO_DATA)
    {
        row = (ERL_NIF_TERM *)malloc(sizeof(ERL_NIF_TERM) * (counter - 1));
        for (i = 0; i < counter - 1; i++)
        {
            memset(&row[i], 0, sizeof(ERL_NIF_TERM));

            switch(cols[i]->dtype)
            {
                case SQLT_VCS:  /* var char */
                case SQLT_CHR:  /* char */
                case SQLT_STR: 
                case SQLT_DAT:
                    row[i] = enif_make_string(env, data[i], ERL_NIF_LATIN1);
                    break;
                case SQLT_INT:
                case SQLT_NUM:
                    row[i] = enif_make_int(env, atoi(data[i]));
                    break;
/*                case SQLT_DAT:
                    lstat = OCIDateTimeToText((dvoid *)envhp, errhp,
                            (OCIDateTime *)data[i], (text *)NULL, (ub1) 0, 
                            (ub2) 2, (text *)NULL, (ub4) 0, (ub4 *)16, buf);
                    if (lstat) return check_error(env, lstat);
                    row[i] = enif_make_string(env, buf, ERL_NIF_LATIN1);
*/
                default:
                    row[i] = atom_ok;
                    break;
            }
        }
        rows = (ERL_NIF_TERM *)realloc(rows, sizeof(ERL_NIF_TERM) * rownum + 1);
        memset(&rows[rownum], 0, sizeof(ERL_NIF_TERM));
        rows[rownum] = enif_make_tuple_from_array(env, row, counter - 1);

        free((ERL_NIF_TERM *)row);

        lstat = OCIStmtFetch(stmthp, errhp, 1, OCI_FETCH_NEXT, OCI_DEFAULT);
        rownum++;
    }
    /* {{c1, c2, c3}, [{1,2,3}, {1,2,3}, {1,2,3}]} */
    ERL_NIF_TERM col_names[counter - 1];
    for(i = 0; i < counter - 1; i++) {
        col_names[i] = cols[i]->column_name;
        free((char *)data[i]);
    }

    ret = enif_make_tuple2(env,
            enif_make_tuple_from_array(env, col_names, counter - 1),
            enif_make_list_from_array(env, rows, rownum));

    OCIHandleFree((dvoid *)stmthp, OCI_HTYPE_STMT);
    free((ERL_NIF_TERM *)columns);
    free((ERL_NIF_TERM *)rows);

    return ret;
}

static ERL_NIF_TERM
ora_disconnect(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    
    sb4 lstat = 0;
    ERL_NIF_TERM ret;

    lstat = destroy_connection();

    if (lstat)
        ret = check_error(env, lstat);
    else {
        is_connected = 0;
        ret = atom_ok;
    }

    cleanups();
    return ret;
}

static ERL_NIF_TERM check_connection(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    return is_connected ? atom_true : atom_false;
}

/* Internal functions */
static int create_connection(CONST OraText *username,
                                    CONST OraText *password,
                                    CONST OraText *database)
{
    sb4 lstat = 0;

    /* Initialize oracle handlers */
    OCIEnvCreate((OCIEnv **) &envhp, (ub4) OCI_DEFAULT,
                  (dvoid *) 0, (dvoid * (*)(dvoid *,size_t)) 0,
                  (dvoid * (*)(dvoid *, dvoid *, size_t)) 0,
                  (void (*)(dvoid *, dvoid *)) 0, (size_t) 0, (dvoid **) 0);

    OCIHandleAlloc((dvoid *) envhp, (dvoid **) &errhp, OCI_HTYPE_ERROR,
            (size_t) 0, (dvoid **) 0);
    OCIHandleAlloc((dvoid *) envhp, (dvoid **) &svchp, OCI_HTYPE_SVCCTX,
            (size_t) 0, (dvoid **) 0);

    lstat = OCILogon2(envhp, errhp, &svchp,
            username, (ub4)strlen((char *)username),
            password, (ub4)strlen((char *)password),
            database, (ub4)strlen((char *)database),
            OCI_DEFAULT);

    return lstat;
}

static int
destroy_connection(void)
{
    return OCILogoff(svchp, errhp);
}

static ERL_NIF_TERM
make_error_tuple(ErlNifEnv *env, struct oci_error *err)
{
    ERL_NIF_TERM tuple;

    tuple = enif_make_tuple2(env,
                             enif_make_int(env, err->code),
                             enif_make_string(env, err->message, ERL_NIF_LATIN1));

    return enif_make_tuple2(env, atom_error, tuple);
}

static ERL_NIF_TERM check_error(ErlNifEnv *env, sb4 status)
{
    struct oci_error *err = (struct oci_error *)malloc(sizeof(struct oci_error));
    memset(err, 0, sizeof(struct oci_error));

    switch(status)
    {
        case OCI_SUCCESS_WITH_INFO:
            strcpy(err->message, "OCI Success with info");
            break;
        case OCI_NEED_DATA:
            strcpy(err->message, "OCI Need data");
            break;
        case OCI_NO_DATA:
            strcpy(err->message, "OCI No data");
            break;
        case OCI_INVALID_HANDLE:
            strcpy(err->message, "OCI Invalid handle");
            break;
        case OCI_STILL_EXECUTING:
            strcpy(err->message, "OCI Still execute");
            break;
        case OCI_CONTINUE:
            strcpy(err->message, "OCI Continue");
            break;
        case OCI_ERROR:
            OCIErrorGet((dvoid *)errhp, (ub4)1, (text *)NULL, &err->code,
                        err->message, (ub4)sizeof(err->message), OCI_HTYPE_ERROR);
            /* remove \n from end of line */
            err->message[strlen(err->message) - 1] = '\0';
            break;
        default:
            return enif_make_tuple2(env, atom_error,
                    enif_make_tuple2(env, enif_make_int(env, 0), atom_undefined));
    }
    return make_error_tuple(env, err);
}

static void cleanups()
{
    OCIHandleFree((dvoid *) envhp, OCI_HTYPE_ENV);
    OCIHandleFree((dvoid *) errhp, OCI_HTYPE_ERROR);
    OCIHandleFree((dvoid *) svchp, OCI_HTYPE_SVCCTX);
}

