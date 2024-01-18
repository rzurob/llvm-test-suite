! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 2011-07-19
!*  ORIGIN                     :
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : 1st test case reported in defect 378466.
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012
   real, save :: x(10)[*]

   call foo (x(2:4)) !<-- this is legal
   call foo (x(::2)) !<-- illegal

   call bar (3, x([1, 3, 2]))  !<-- illegal

   contains

   subroutine foo (r)
      real r(3)[*]

      r = 1.0
   end subroutine

    subroutine bar (n,x1)
        integer, intent(in) :: n
        real, intent(in), dimension(*), codimension[n,*] :: x1

        if (this_image() == 1) print *, x1(1:n)
    end subroutine
end
