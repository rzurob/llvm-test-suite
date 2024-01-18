! GB DTP extension using:
! ftcx_dtp -qck -qk -ql /tstdev/OO_type/abstract/C611/proceRef002.f
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
! %GROUP: redherring.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD: dcomp proceRef002.f
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
!*  DATE                       : 09/28/2004
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*                             :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DRIVER STANZA              : xlf95
!*
!*  DESCRIPTION                : Testing: If the rightmost part-name is of abstract type, data-ref shall be polymorphic. (C611)
!*                                        non-polymorphic non-rightmost array object call type bound procedures
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m

   type, abstract :: base(k1)    ! (4)
      integer, kind :: k1
      integer(k1)   :: id
   contains
      procedure, nopass :: printtype => printbase
   end type

   type, extends(base) :: child(k2)    ! (4,4)
      integer, kind :: k2
      integer(k2)   :: rid
   contains
      procedure, nopass :: printtype => printchild
   end type

   interface
      subroutine printif()
      end subroutine
   end interface

contains

   subroutine printchild()
      print *,'child'
   end subroutine

   subroutine printbase()
      print *,'base'
   end subroutine

end module

program proceRef002
   use m

   type(child(4,4)), dimension(2) :: c1 = (/ child(4,4) (1,2),child(4,4) (4,5) /)
   class(child(4,4)), allocatable, dimension(:) :: c2
   class(child(4,4)), pointer, dimension(:) :: c3

   allocate( c2(2), source = (/ child(4,4) (7,8),child(4,4) (9,10) /) )
   allocate( c3(2), source = (/ child(4,4) (0,1),child(4,4) (2,3) /) )

   call c1%base%printtype()
   call c2%base%printtype()
   call c3%base%printtype()

   call c1(1:1)%base%printtype()
   call c2(2:1:-1)%base%printtype()
   call c3(1:2)%base%printtype()

end program
