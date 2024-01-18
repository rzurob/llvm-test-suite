!*  =================================================================== 
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY 
!*  =================================================================== 
!*  =================================================================== 
!*
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Huiwen Li
!*  DATE                       : 07/25/2006
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*                             :
!*  SECONDARY FUNCTIONS TESTED : 
!*
!*  DRIVER STANZA              : xlf2003
!*
!*  DESCRIPTION                : ALLOCATE with DTP
!*                               (type-spec uses selected_int_kind intrinsic)
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!* ==================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

program dtParamAlloc05

    type base (k, n)
        integer, kind :: k
        integer, len  :: n

        integer(k) :: data(n) = 0
    end type

    class(*), pointer :: typtr

    type(base(selected_int_kind(10), 20)) :: typvar

    allocate(base(selected_int_kind(10), 20) :: typtr)

    if ( (sizeof(typvar) .eq. 0) .or. &
         (sizeof(typvar) /= sizeof(typvar)) ) then
       stop 1
    endif

end
