!*  ===================================================================
!*
!*  DIAGNOSTIC TESTED          : using sizeof() on automatic object
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!* ===================================================================
!234567890123456789012345678901234567890123456789012345678901234567890

    type base (k, n)
        integer, kind :: k
        integer, len  :: n

        integer(k) :: data(n) = 0
    end type

    call sub1(20)
    contains
      subroutine sub1(len_tp)
        integer len_tp
        type(base(4, len_tp)) :: local_var
        if (local_var%k /= 4) error stop 1
        if (local_var%n /= 20) error stop 2
        if (sizeof(local_var) .ne. 80) error stop 8
      end subroutine

end
