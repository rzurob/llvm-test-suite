/* ********************************************************************** */
/* ********************************************************************** */

typedef enum
  {
    XLPGAS_DT_byte,    /**<  8 bits unsigned */
    XLPGAS_DT_chr,     /**<  8 bits signed   */
    XLPGAS_DT_srt,     /**< 16 bits signed   */
    XLPGAS_DT_hwrd,    /**< 16 bits unsigned */
    XLPGAS_DT_int,     /**< 32 bits signed   */
    XLPGAS_DT_word,    /**< 32 bits unsigned */
    XLPGAS_DT_llg,     /**< 64 bits signed   */
    XLPGAS_DT_dwrd,    /**< 64 bits unsigned */
    XLPGAS_DT_dbl,     /**< 64 bits, IEEE fp */
    XLPGAS_DT_ldbl,     /**< 128 bits, long double */
    XLPGAS_DT_flt,     /**< 32 bits, IEEE fp */
    XLPGAS_DT_dblint   /**< 96 bits, dbl+int */
  }
  xlpgas_dtypes_t;

typedef enum
  {
    XLPGAS_OP_ADD = 0,       /**< addition */
    XLPGAS_OP_MUL,           /**< multiplication */
    XLPGAS_OP_DIV,           /**< division */
    XLPGAS_OP_AND,           /**< bitwise and */
    XLPGAS_OP_OR,            /**< bitwise or */
    XLPGAS_OP_XOR,           /**< bitwise xor */
    XLPGAS_OP_LOGAND,        /**< logical and */
    XLPGAS_OP_LOGOR,         /**< logical or */
    XLPGAS_OP_MAX,           /**< maximum */
    XLPGAS_OP_MIN,            /**< minimum */
    XLPGAS_OP_UFUNC,
    XLPGAS_OP_NONC_UFUNC
  }
  xlpgas_ops_t;

void kmeans_barrier ()
{
  xlpgas_tspcoll_barrier(xlpgas_tsp_thrd_id(), 0);
}

void kmeans_reduce_dbl (void * buf, long long int * count)
{
  xlpgas_tspcoll_allreduce (xlpgas_tsp_thrd_id(), 0, buf, buf, 
                               XLPGAS_OP_ADD,
                               XLPGAS_DT_ldbl,
			       (int ) (* count));
}

void kmeans_reduce_int (void * buf, long long int * count)
{
  xlpgas_tspcoll_allreduce (xlpgas_tsp_thrd_id(), 0, buf, buf,
			       XLPGAS_OP_ADD,
                               XLPGAS_DT_llg,
			       (int) (* count));
}

void kmeans_reduce_max (void *buf, long long int * count)
{
  xlpgas_tspcoll_allreduce (xlpgas_tsp_thrd_id(), 0, buf, buf, 
			       XLPGAS_OP_MAX,
			       XLPGAS_DT_ldbl,
			       (int) (*count));
}

