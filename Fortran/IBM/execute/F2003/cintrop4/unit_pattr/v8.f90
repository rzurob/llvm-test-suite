!*********************************************************************
!***********************************************************************

  !!! Fortran interface with args defined by ISO_C_BINDING types.
  !!! [COPIED FROM F2k standard]

    INTERFACE
      ! Interoperable with the C function prototype:
      !   short func (int i, double *j, int *k, int m[10], void *n);
      FUNCTION func (i, j, k, m, n) BIND(C)
        USE ISO_C_BINDING

        INTEGER(C_SHORT) :: func
        INTEGER(C_INT), VALUE :: i
        REAL(C_DOUBLE) :: j
        INTEGER(C_INT) :: k, m(10)
        TYPE(C_PTR), VALUE :: n
      END FUNCTION func
    END INTERFACE
  END
