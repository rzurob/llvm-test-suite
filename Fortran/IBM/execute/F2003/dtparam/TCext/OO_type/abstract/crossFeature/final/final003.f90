! GB DTP extension using:
! ftcx_dtp -qck -qk -ql /tstdev/OO_type/abstract/crossFeature/final/final003.f
! SCCS ID Information
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: final003.f
! %VERIFY: final003.out:final003.vf
! %STDIN:
! %STDOUT: final003.out
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 09/28/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : Testing: Final Subroutines
!*                               Base non-abstract type contains a final procedure, extension polymorphic abstract type with no final
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
   type :: base(k1)    ! (4)
      integer, kind :: k1
      integer(k1)   :: i
   contains
      final :: finalbase1, finalbase2
   end type

   type, extends(base), abstract :: child(k2)    ! (4,4)
      integer, kind :: k2
      integer(k2)   :: j
   end type

   type, extends(child) :: gen3(k3)    ! (4,4,4)
      integer, kind :: k3
      integer(k3)   :: k
   contains
      final :: finalgen31, finalgen32
   end type

contains

   subroutine finalbase1(a)
      type(base(4)), intent(inout) :: a
      print *,"finalizebasescalar", a%i
      a%i = 0
   end subroutine

   subroutine finalbase2(a)
      type(base(4)), intent(inout) :: a(:)
      print *,"finalizebasearray", a%i
      a%i = 0
   end subroutine

   subroutine finalgen31(a)
      type(gen3(4,4,4)), intent(inout) :: a
      print *,"finalizegen3scalar", a%i, a%j, a%k
      a%j = 0
      a%k=0
   end subroutine

   subroutine finalgen32(a)
      type(gen3(4,4,4)), intent(inout) :: a(:)
      print *,"finalizegen3array", a%i, a%j, a%k
      a%j = 0
      a%k =0
   end subroutine

end module

program final003
   use m

   class(child(4,4)), allocatable :: b1
   class(child(4,4)), allocatable, dimension(:) :: b2

   allocate (b1, source = gen3(4,4,4)(5,6,7))
   allocate (b2(2), source = (/gen3(4,4,4)(1,2,3),gen3(4,4,4)(3,4,5)/))

   deallocate(b1,b2)

end program


