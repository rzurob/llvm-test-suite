!#######################################################################
! SCCS ID Information
! %W%, %I%
! Extract Date/Time: %D% %T%
! Checkin Date/Time: %E% %U%
!#######################################################################
!***********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS: -qfree=f90 
! %GROUP: fxso16r.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD: 
! %END
!**********************************************************************
!**********************************************************************
!*  ===================================================================
!*  AIX XL FORTRAN/6000 TEST CASE                 IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : fxso16r.f
!*  TEST CASE TITLE            : SUBSCRIPTORDER Directive 
!*
!*  PROGRAMMER                 : Gregory Matviyiv
!*  DATE                       : April 16, 2002
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : SUBSCRIPTORDER Directive
!*
!*  DESCRIPTION                : This program tests SUBSCRIPTORDER Directive for
!*                               real 2-dim arrays with pure subroutine
!*                               in interface block with assumed shape array
!*                               in parameters.
!*
!*
!* ===================================================================
!*  REVISION HISTORY
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!234567890123456789012345678901234567890123456789012345678901234567890

program abstracti061
!*********************************************************************
!  Global Variables
!
implicit none
integer :: i
integer :: i1, i2
logical :: log_var = .false.
logical :: precision_r4
!
!*********************************************************************
!  Parameters for lower and upper bounds of arrays for each dimensions.
!
integer,   parameter :: l1 = -30, u1 = -10
integer,   parameter :: l2 = -22, u2 =  12
!
!*********************************************************************
!  Parameters for real constants used for array initialization.
!
real*4,    parameter :: r4_c  = 1.0E+01
!
!*********************************************************************
!  Using SUBSCRIPTORDER directive for the array.
!
!IBM* subscriptorder ( arr(2,1))
!
!*********************************************************************
!  Declaration of explicit arrays of rank 2.
!
real*4  arr (l1:u1,l2:u2), arrt (l2:u2,l1:u1)
!
!
abstract interface
   pure function plus_lb1_ai(array)
      real, intent(in) :: array(:,:)
      real :: plus_lb1_ai(size(array,dim=1),size(array,dim=2))
   end function
end interface

procedure (plus_lb1_ai) :: plus_lb1
!
!*********************************************************************
! Initialization of the arrays.
!
i = 1
do i1 = l1, u1
   do i2 = l2, u2
      arr(i1,i2) = r4_c * i
      arrt(i2,i1) = r4_c * i
      i = i + 1
   end do
end do
!
! *****************************************************************************
!   Test : assign values into rearranged by subscriptorder directives arr
!          and into rearranged manually array arrt. 
!
! 
arr(:,:) = plus_lb1(arr(:,:))
arrt(:,:) = plus_lb1(arrt(:,:))
!
!*********************************************************************
!  Check the values of arrays
!
do i1 = l1, u1
   do i2 = l2, u2
     if (.not. precision_r4(arr(i1,i2), arrt(i2,i1))) then
       call zzrc ( 1 )
     endif       
   end do
end do
!
!
end program abstracti061
!
!   
! ********************* FUNCTIONS AND SUBROUTINES ********************
!
!
pure function plus_lb1(array)
      real, intent(in) :: array(:,:)
      real :: plus_lb1(size(array,dim=1),size(array,dim=2)), x
    interface
       pure subroutine sin_ub2(arra)
          real, intent(inout) :: arra(:,:)
       end subroutine
     end interface
     x = 8.0
     plus_lb1(lbound(array,dim=1):,:) = array(lbound(array,dim=1):,:) + x
     call sin_ub2(plus_lb1)
end function
!
! *****************************************************************************
!
pure subroutine sin_ub2(arra)
     real, intent(inout) :: arra(:,:)
     real y
     y = 6.0
     arra(:,ubound(arra,dim=2)) = sin(arra(:,ubound(arra,dim=2))) + y 
end subroutine


