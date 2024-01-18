!#######################################################################
! SCCS ID Information
! %W%, %I%
! Extract Date/Time: %D% %T%
! Checkin Date/Time: %E% %U%
!#######################################################################
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: funcRetrn002.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD: 
! %END
! *********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*  ===================================================================
!*
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Robert Ma
!*  DATE                       : 11/08/2004
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*                             :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DRIVER STANZA              : xlf95
!*
!*  DESCRIPTION                : Testing: Section 9.9.3: Inquire by output list
!*                               - Try output item function return
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m1
   type base
      character(3) :: c = ''
   contains
      procedure, pass :: getBase
   end type
   
contains
   elemental function getBase(a)
      class(base), intent(in) :: a
      type(base) :: getBase
      select type (a)
         type is (base)
            getBase = a
      end select   
   end function
end module

program funcRetrn002
   use m1   
   
   interface 
      elemental function getBase1(a)
         import base
         type(base)  :: getBase1
         class(base), intent(in) :: a
      end function
   end interface
     
   ! declaration of variables
   class(base), allocatable :: b1(:), b2(:,:)
   class(base), pointer :: b3(:), b4(:,:) 
   integer :: length1 = 0 
   
   ! allocation of variables
   
   allocate(b1(4), source = (/ base('abc'), base('def'), base('ghi'), base('jkl') /) )
   allocate(b2(2,2), source = reshape( source = (/ base('abc'), base('def'), base('ghi'), base('jkl') /), shape=(/2,2/) ))
   allocate(b3(4), source = (/ base('ABC'), base('DEF'), base('GHI'), base('JKL') /) )
   allocate(b4(2,2), source = reshape( source = (/ base('ABC'), base('DEF'), base('GHI'), base('JKL') /), shape=(/2,2/) ))
   
   inquire (iolength = length1)   b1%getBase()
   if ( length1 /= 12 ) error stop 1_4
   length1=0
   inquire (iolength = length1)   b2%getBase()
   if ( length1 /= 12 ) error stop 2_4
   length1=0
   inquire (iolength = length1)  getBase1(b3), b3%getBase()
   if ( length1 /= 24 ) error stop 3_4
   length1=0
   
   select type ( b14 => b4 )
      type is (base)
         inquire (iolength = length1)  b14%getBase(),getBase1(b14)
         if ( length1 /= 24 ) error stop 4_4
   end select
   
end program

elemental function getBase1(a)
   use m1
   type(base)  :: getBase1
   class(base), intent(in) :: a

   select type (a)
      type is (base)
         getBase1 = a
   end select   
end function