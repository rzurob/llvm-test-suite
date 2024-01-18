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
! %GROUP: elemental001.f
! %VERIFY: elemental001.out:elemental001.vf
! %STDIN: 
! %STDOUT: elemental001.out
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
!*                               - Inquire inside an elemental procedure
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
         procedure, pass :: checkIOLength
   end type
   
   type, extends(base) :: child
      integer(8)   :: i = 0
   end type
   
   
contains
   
   elemental integer function checkIOlength(a)
      class(base), intent(in) :: a
      integer :: i1
      select type (a)
         type is (base)
            inquire ( iolength = i1 ) a
         type is (child)
            inquire ( iolength = i1 ) a
      end select
      
      checkIOlength = i1
      
   end function
   
end module

program elemental001
   use m1   
     
   ! declaration of variables
   class(base), allocatable :: b1(:), b2(:,:)
   class(base), pointer :: b3(:), b4(:,:)   
   integer :: length1 = 0 
   
   ! allocation of variables
   
   allocate(b1(4), source = (/ base('abc'), base('def'), base('ghi'), base('jkl') /) )
   allocate(b2(2,2), source = reshape( source = (/ child('abc',1), child('def',2), child('ghi',3), child('jkl',4) /), shape=(/2,2/) ))
   allocate(b3(4), source = (/ child('ABC',1), child('DEF',2), child('GHI',3), child('JKL',4) /) )
   allocate(b4(2,2), source = reshape( source = (/ base('ABC'), base('DEF'), base('GHI'), base('JKL') /), shape=(/2,2/) ))
   
   print *,b1%checkIOlength()
   print *,b2%checkIOlength()
   print *,b3%checkIOlength()
   print *,b4%checkIOlength()

end program

