!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*  ===================================================================
!*
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Robert Ma
!*  DATE                       : 11/01/2005
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   : Section 4.5.4: Generic Type Bound Procedure
!*                             :
!*  SECONDARY FUNCTIONS TESTED : with Assignment(=)
!*
!*  DRIVER STANZA              : xlf2003
!*
!*  DESCRIPTION                : Derived Type Intrinsic Assignment:
!*                                 - Deeper levels of allocatable components have elemental generic assignment defined
!*
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

   type level4
      integer :: i
      contains
         generic :: assignment(=) => l4assgn
         procedure :: l4assgn => assgn
   end type

   type level3
      integer :: i
      type(level4), allocatable :: l4(:)
      contains
         procedure :: writeformatted3
         generic :: write(formatted) => writeformatted3
   end type

   type level2
      integer :: i
      type(level3), allocatable :: l3(:)
      contains
         procedure :: writeformatted2
         generic :: write(formatted) => writeformatted2
   end type

   type level1
      integer :: i
      type(level2), allocatable :: l2(:)
      contains
         procedure :: writeformatted1
         generic :: write(formatted) => writeformatted1
   end type

   type level0
      integer :: i
      type(level1) :: l1
      contains
         procedure :: writeformatted0
         generic :: write(formatted) => writeformatted0
   end type

   contains

      elemental subroutine assgn ( a, b )
         class(level4), intent(out) :: a
         class(level4), intent(in)  :: b

         a%i = b%i + 1

      end subroutine

      subroutine writeformatted0(dtv, unit, iotype, v_list, iostat, iomsg )
         class(level0), intent(in) :: dtv
         integer,  intent(in) :: unit
         character(*), intent(in) :: iotype
         integer, intent(in)     :: v_list(:)
         integer,  intent(out) :: iostat
         character(*),  intent(inout) :: iomsg

         write ( unit, * , iostat = iostat, iomsg = iomsg ) dtv%i, dtv%l1

      end subroutine

      subroutine writeformatted1(dtv, unit, iotype, v_list, iostat, iomsg )
         class(level1), intent(in) :: dtv
         integer,  intent(in) :: unit
         character(*), intent(in) :: iotype
         integer, intent(in)     :: v_list(:)
         integer,  intent(out) :: iostat
         character(*),  intent(inout) :: iomsg

         write ( unit, * , iostat = iostat, iomsg = iomsg ) dtv%i, dtv%l2

      end subroutine

      subroutine writeformatted2(dtv, unit, iotype, v_list, iostat, iomsg )
         class(level2), intent(in) :: dtv
         integer,  intent(in) :: unit
         character(*), intent(in) :: iotype
         integer, intent(in)     :: v_list(:)
         integer,  intent(out) :: iostat
         character(*),  intent(inout) :: iomsg

         write ( unit, * , iostat = iostat, iomsg = iomsg ) dtv%i, dtv%l3

      end subroutine

      subroutine writeformatted3(dtv, unit, iotype, v_list, iostat, iomsg )
         class(level3), intent(in) :: dtv
         integer,  intent(in) :: unit
         character(*), intent(in) :: iotype
         integer, intent(in)     :: v_list(:)
         integer,  intent(out) :: iostat
         character(*),  intent(inout) :: iomsg

         write ( unit, * , iostat = iostat, iomsg = iomsg ) dtv%i, dtv%l4

      end subroutine

end module

program genericAssignmentDtIntrinAssgn041
   use m

   type(level0) :: l01, l02, l03

   type(level1) :: l10
   type(level2) :: l20
   type(level3) :: l30

   allocatable :: l02
   pointer :: l03

   allocate ( l02, l03 )

   l01 = level0( 1, level1(2, (/ level2(3, (/ level3( 4, (/ level4 ( 5 ), level4(6) /) ) /) ), level2(7, (/ level3( 8, (/ level4 ( 9 ), level4(10) /) ) /) ) /) ) )
   print *, l01

   l02 = l01
   print *, l02

   l03 = l02
   print *, l03

   l10 = l01%l1
   print *, l10

   l20 = l01%l1%l2(1)
   print *, l20

   l30 = l02%l1%l2(1)%l3(1)
   print *, l30

end program
