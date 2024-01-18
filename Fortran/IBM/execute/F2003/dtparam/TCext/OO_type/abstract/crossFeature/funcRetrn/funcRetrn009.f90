! GB DTP extension using:
! ftcx_dtp -qck -qk -ql /tstdev/OO_type/abstract/crossFeature/funcRetrn/funcRetrn009.f
! SCCS ID Information
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: funcRetrn009.f
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
!*  DESCRIPTION                : Testing: Function subprogram (Section 12.5.2.1), class(abstract type)
!*                                        returns polymorphic abstract base type array in subfunction
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
   end type

   type, extends(base) :: child(k2,n1)    ! (4,4,20)
       integer, kind :: k2
       integer, len  :: n1
   end type


contains

   class(base(4)) function foo(a)
      pointer :: foo(:)
      class(base(4)), intent(in) :: a(:)
      allocate(foo(size(a)), source=innerfoo(a) )
   contains
      function innerfoo(a) result(boo)
         class(base(4)), pointer :: boo(:)
         class(base(4)), intent(in) :: a(:)
         allocate(boo(size(a)), source=a)
      end function
   end function

end module

program funcRetrn009
   use m

   class(base(4)), allocatable :: c(:)
   class(base(4)), allocatable :: b1(:)
   allocate (b1(2), source = (/ child(4,4,20)(4), child(4,4,20)(5) /))
   allocate ( c(2),source=foo(b1) )

   if ( c(1)%id .ne. 4) error stop 1_4
   if ( c(2)%id .ne. 5) error stop 2_4

end program

