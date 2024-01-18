! GB DTP extension using:
! ftcx_dtp -qck -qk -ql /tstdev/OO_type/abstract/crossFeature/funcRetrn/funcRetrn007.f
! with manual adjustment for specification expression (a%n1, a%n2)
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 09/28/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : Testing: Function subprogram (Section 12.5.2.1), class(abstract type)
!*                                        returns polymorphic array abstract non-base type
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

   type, abstract:: base(k1)    ! (4)
      integer, kind :: k1
      integer(k1)   :: id
   end type

   type, extends(base) :: child(k2,n1)    ! (4,4,20)
       integer, kind :: k2
       integer, len  :: n1
   end type

   type, extends(child), abstract :: gen3(k3,n2)    ! (4,4,20,4,20)
       integer, kind :: k3
       integer, len  :: n2
   end type

   type, extends(gen3) :: gen4(k4,n3)    ! (4,4,20,4,20,4,20)
       integer, kind :: k4
       integer, len  :: n3
   end type

contains

   function foo(a)
      class(gen3(4,4,*,4,*)), intent(in), dimension(:) :: a

      class(gen3(4,4,a%n1,4,a%n2)), pointer:: foo(:)
      allocate(foo(size(a)), source = a)
   end function

   function foo1(a) result (boo)
      class(gen3(4,4,*,4,*)), intent(in), dimension(:) :: a
      class(gen3(4,4,a%n1,4,a%n2)), pointer, dimension(:) :: boo
      allocate(boo(size(a)), source=a)
   end function

end module

program funcRetrn007
   use m

   class(base(4)), pointer, dimension(:) :: c
   class(gen3(4,4,20,4,20)), allocatable, dimension(:) :: g1

   allocate(g1(2), source = (/ (gen4(4,4,20,4,20,4,20)(i),i=4,5) /))

   c => foo1(g1)
   if (c(1)%id .ne. 4) error stop 1_4
   if (c(2)%id .ne. 5) error stop 2_4

   deallocate(c)

   allocate ( c(2), source = foo((/ (gen4(4,4,20,4,20,4,20)(i),i=7,8) /)))
   if (c(1)%id .ne. 7) error stop 3_4
   if (c(2)%id .ne. 8) error stop 4_4

end program

