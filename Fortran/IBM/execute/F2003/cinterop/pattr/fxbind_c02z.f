! *********************************************************************
!**********************************************************************
!* ===================================================================
!*
!* DATE                         : Jan. 1, 2004
!*
!* PRIMARY FUNCTIONS TESTED     :
!* SECONDARY FUNTIONS TESTED
!*
!* REQUIRED COMPILER OPTIONS    :
!*
!* DESCRIPTION                  : Test: BINC(C) attribute
!*                                with C_ptr type of integer, real
!*                                logical(1), character(1). Using module
!*                                subroutine,interface.Fortran calls C.
!* ===================================================================
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!*  03/05/03    SK     -Initial Version
!* ===================================================================
!*
!234567890123456789012345678901234567890123456789012345678901234567890

module m
   use iso_c_binding

   implicit none

   interface
       subroutine extsub_int(i1) bind(c)
           use iso_c_binding
           type(C_PTR) :: i1
       end subroutine extsub_int

       subroutine extsub_real(r4) bind(c)
           use iso_c_binding
           type(c_ptr) :: r4
       end subroutine extsub_real

       subroutine extsub_log(l1) bind(c)
           use iso_c_binding
           type(C_PTR) ::  l1
       end subroutine extsub_log

       subroutine extsub_char(ch1) bind(c)
           use iso_c_binding
           type(C_PTR) ::  ch1
       end subroutine extsub_char

   end interface
end module m

   use m

   logical precision_R4, precision_R6, precision_R8
   logical precision_x8, precision_x16

!**********************************************************
!        Initialization of variables                      *
!**********************************************************

   integer*1, target :: ai1, bi1

   real*4, target :: ar4, br4

   logical*1, target ::  al1 , bl1


   character*1, target :: ach1 , bch1

   type(C_PTR) :: cp

   ai1 =  0
   bi1 =  15

   ar4 = 4.80
   br4 = 9.60

   al1 = .true.
   bl1 = .false.

   ach1 = 'd'
   bch1 = 'a'


!**********************************************************
!        Calling C from Fortran with integer data type
!                and check the results
!**********************************************************

   cp = c_loc(ai1)

   call extsub_int(cp)
      if(ai1 .ne. bi1)then
        error stop 10
      endif

!**********************************************************
!        Calling C from Fortran with logical data type
!                and check the Results
!**********************************************************

   cp = c_loc(al1)

   call extsub_log(cp)
      if(al1 .neqv. bl1)then
        error stop 30
      endif


!**********************************************************
!        Calling C from Fortran with character data type
!                and check the Results
!**********************************************************

   cp = c_loc(ach1)

   call extsub_char(cp)
      if(ach1 .ne. bch1)then
        error stop 50
      endif

   cp = c_loc(ar4)

!**********************************************************
!        Calling C from Fortran with real data type
!                and check the Results
!**********************************************************


   call extsub_real(cp)
      if (.not. precision_r4(ar4, br4)) then
         error stop 60
      end if


end
