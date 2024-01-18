! GB DTP extension using:
! ftcx_dtp -qck -qk -ql /tstdev/OO_type/abstract/crossFeature/typeBound/typeBound001.f
! SCCS ID Information
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: typeBound001.f
! %VERIFY:
! %STDIN:
! %STDOUT:
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
!*  DESCRIPTION                : Testing: Type-bound procedure - Polymorphic abstract type call its own type bound
!*                                        Extension type of the abstract type calling the abstract type's non-deferred type bound
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
   type , abstract:: base(k1)    ! (4)
      integer, kind :: k1
      integer(k1)   :: i = 5
   contains
      procedure, pass :: print => printbase
   end type

   type, extends(base) :: child(k2,n1)    ! (4,4,20)
       integer, kind :: k2
       integer, len  :: n1
   end type

   type, extends(child), abstract :: gen3(k3)    ! (4,4,20,4)
     integer, kind :: k3
     integer(k3)   :: j = 15
   contains
      procedure, pass :: printgen3 => printgen3
   end type

   type, extends(gen3) :: gen4(k4,n2)    ! (4,4,20,4,4,20)
       integer, kind :: k4
       integer, len  :: n2
   end type

contains

   integer function printbase(a)
      class(base(4)), intent(in) :: a
      printbase = a%i
   end function

   integer function printgen3(a)
      class(gen3(4,4,*,4)), intent(in) :: a
      printgen3 = a%j
   end function

end module

program typeBound001
   use m

   class(base(4)), allocatable, target :: b1
   class(base(4)), pointer             :: b2
   class(gen3(4,4,20,4)), allocatable, target :: g1

   allocate (b1, source = child(4,4,20)(7) )
   allocate (b2, source = gen4(4,4,20,4,4,20)(1,2))

   allocate(g1, source = gen4(4,4,20,4,4,20)(4,9) )

   if ( b1%print() .ne. 7 )      error stop 1_4
   if ( b2%print() .ne. 1 )      error stop 2_4
   if ( g1%print() .ne. 4 )      error stop 3_4
   if ( g1%printgen3() .ne. 9 )  error stop 4_4

end program



