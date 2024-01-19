!*  ===================================================================
!*
!*  DATE                       : 11/01/2005
!*
!*  PRIMARY FUNCTIONS TESTED   : Section 4.5.4: Generic Type Bound Procedure
!*  SECONDARY FUNCTIONS TESTED : with Assignment(=)
!*
!*  DESCRIPTION                : Derived Type Intrinsic Assignment:
!*                                 - more components (scalar and array) and several have generic assignment defined
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


   type com1
      integer i
      contains
         procedure :: c1a
         generic :: assignment(=) => c1a
   end type

   type com2
      integer i
      contains
         procedure :: c2a
         generic :: assignment(=) => c2a
   end type

   type, extends(com2) :: c_com2
      integer :: j
      contains
         procedure :: c2a => cc2a
   end type

   type com3
      sequence
      integer i
   end type

   contains

      elemental subroutine c1a ( a, b )
         class(com1) , intent(out) :: a
         class(com1) , intent(in)  :: b

         a%i = b%i + 1

      end subroutine

      elemental subroutine c2a ( a, b )
         class(com2) , intent(out) :: a
         class(com2) , intent(in)  :: b

         a%i = b%i + 1

      end subroutine

      elemental subroutine cc2a ( a, b )
         class(c_com2) , intent(out) :: a
         class(com2) , intent(in)  :: b

         a%i = b%i + 1

         select type ( b )
            type is ( c_com2 )
               a%j = b%j + 1
         end select

      end subroutine

end module

module n
   use m, only: com1, com2, c_com2, com3

   type container
      character(3) :: c1
      type(com1) :: c11(3)
      integer :: i(2)
      class(com1), allocatable :: c12(:)
      type(com2), allocatable  :: c21(:)
      class(com2), allocatable :: c22(:)
      type(c_com2) :: cc21(2)
      type(com3) :: c31(2)
      type(com3), allocatable :: c32(:)
      contains
         procedure :: writeformatted
         generic :: write(formatted) => writeformatted
   end type

   contains

      subroutine writeformatted(dtv, unit, iotype, v_list, iostat, iomsg )
         class(container), intent(in) :: dtv
         integer,  intent(in) :: unit
         character(*), intent(in) :: iotype
         integer, intent(in)     :: v_list(:)
         integer,  intent(out) :: iostat
         character(*),  intent(inout) :: iomsg

         write ( unit, * , iostat = iostat, iomsg = iomsg ) dtv%c1, dtv%c11%i, dtv%i, dtv%c12%i, dtv%c21%i
         select type ( g => dtv%c22 )
            type is ( com2 )
               write ( unit, * , iostat = iostat, iomsg = iomsg ) g%i
            type is ( c_com2 )
               write ( unit, * , iostat = iostat, iomsg = iomsg ) g%i, g%j
         end select

         write ( unit, * , iostat = iostat, iomsg = iomsg ) dtv%cc21%i, dtv%cc21%j, dtv%c31%i, dtv%c32%i

      end subroutine

end module

program genericAssignmentDtIntrinAssgn037
  use n

   type(container) :: c1, c2, c3
   allocatable :: c2
   pointer :: c3

   allocate ( c2, c3 )

   c1 = container ( 'abc', (/ com1(1), com1(2), com1(3) /), (/ 4,5 /), (/ com1(6), com1(7), com1(8) /), (/ com2(9), com2(10) /), (/ com2(11), com2(12), com2(13) /), (/ c_com2(14,15), c_com2(16,17) /), (/ com3(18), com3(19) /), (/ com3(20) /) )
   print *, c1

   c2 = c1
   print *, c2

   c3 = c2
   print *, c3

   c2 = container ( 'ABC', (/ com1(11), com1(12), com1(13) /), (/ 14,15 /),(/ com1(16), com1(17), com1(18) /) , (/ com2(19), com2(20) /), (/ c_com2(21,22), c_com2(23,24), c_com2(25,26) /), (/ c_com2(27,28), c_com2(29,30) /), (/ com3(31), com3(32) /), (/ com3(33) /) )
   print *, c2

   c1 = c2
   print *, c1

   c3 = c1
   print *, c3

end program
