! GB DTP extension using:
! ftcx_dtp -qck -qk -ql /tstdev/OO_type/abstract/crossFeature/funcRetrn/funcRetrn010.f
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 09/28/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : Testing: Function subprogram (Section 12.5.2.1), class(abstract type)
!*                                        optional polymorphic abstract type dummy argument
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

   class(base(4)) function foo(a,b)
      class(base(4)), intent(in) :: a(:)
      class(base(4)), optional, intent(in):: b(:)
      pointer :: foo(:)
      if (present(b)) then
         allocate (foo(size(b)), source = b)
      else
         allocate (foo(size(a)), source = a)
      end if
   end function

end module

program funcRetrn010
   use m

   class(base(4)), pointer :: c1(:)
   class(base(4)), pointer :: c2(:)

   allocate(c1(3), source=(/(child(4,4,20)(k),k=1,3)/) )

   c2 => foo(c1)
   if (c2(1)%id .ne. 1) error stop 1_4
   if (c2(2)%id .ne. 2) error stop 2_4
   if (c2(3)%id .ne. 3) error stop 3_4
   if (size(c2) .ne. 3) error stop 4_4

   c1 => foo(c1,(/child(4,4,20)(7)/))
   if (c1(1)%id .ne. 7) error stop 5_4
   if (size(c1) .ne. 1) error stop 6_4

   allocate(c2(2), source = foo((/child(4,4,20)(2)/), (/child(4,4,20)(3), child(4,4,20)(6)/)) )
   if (c2(1)%id .ne. 3) error stop 7_4
   if (c2(2)%id .ne. 6) error stop 8_4
   if (size(c2) .ne. 2) error stop 9_4

end program

