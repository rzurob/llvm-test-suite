!*  ===================================================================
!*
!*  DATE                       : 11/01/2005
!*
!*  PRIMARY FUNCTIONS TESTED   : Section 4.5.4: Generic Type Bound Procedure
!*  SECONDARY FUNCTIONS TESTED : with Assignment(=)
!*
!*  DESCRIPTION                : Derived Type Intrinsic Assignment:
!*                                 - more components and several have generic assignment defined
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

      subroutine c1a ( a, b )
         class(com1) , intent(out) :: a
         class(com1) , intent(in)  :: b

         a%i = b%i

         print *, 'c1a'
      end subroutine

      subroutine c2a ( a, b )
         class(com2) , intent(out) :: a
         class(com2) , intent(in)  :: b

         a%i = b%i

         print *, 'c2a'
      end subroutine

      subroutine cc2a ( a, b )
         class(c_com2) , intent(out) :: a
         class(com2) , intent(in)  :: b

         a%i = b%i

         select type ( b )
            type is ( c_com2 )
               a%j = b%j
         end select

         print *, 'cc2a'
      end subroutine

end module

module n
   use m, only: com1, com2, c_com2, com3

   type container
      character(3) :: c1
      type(com1) :: c11
      integer :: i
      class(com1), allocatable :: c12
      type(com2), allocatable  :: c21
      class(com2), allocatable :: c22
      type(c_com2) :: cc21
      type(com3) :: c31
      type(com3), allocatable :: c32
      contains
         procedure :: writeformatted
         generic :: write(formatted) => writeformatted
         procedure, pass :: return
   end type

   contains

      subroutine writeformatted(dtv, unit, iotype, v_list, iostat, iomsg )
         class(container), intent(in) :: dtv
         integer,  intent(in) :: unit
         character(*), intent(in) :: iotype
         integer, intent(in)     :: v_list(:)
         integer,  intent(out) :: iostat
         character(*),  intent(inout) :: iomsg

         write ( unit, "(a3,4(1x,i4))" , iostat = iostat, iomsg = iomsg, advance = 'no' ) dtv%c1, dtv%c11%i, dtv%i, dtv%c12%i, dtv%c21%i
         select type ( g => dtv%c22 )
            type is ( com2 )
               write ( unit, "(1x,i4)" , iostat = iostat, iomsg = iomsg, advance = 'no' ) g%i
            type is ( c_com2 )
               write ( unit, "(1x,i4,1x,i4)" , iostat = iostat, iomsg = iomsg, advance = 'no' ) g%i, g%j
         end select

         write ( unit, "(4(1x,i4))" , iostat = iostat, iomsg = iomsg, advance = 'no' ) dtv%cc21%i, dtv%cc21%j, dtv%c31%i, dtv%c32%i

      end subroutine

      type(container) function return ( a )
         class(container), intent(in) :: a
         return = a
      end function

end module

program genericAssignmentDtIntrinAssgn036
  use n

   type(container) :: c1, c2, c3
   allocatable :: c2
   pointer :: c3

   allocate ( c2, c3 )

   c1 = container ( 'abc', com1(1), 2, com1(3), com2(4), com2(5), c_com2(6,7), com3(8), com3(9) )
   print *, c1

   c2 = c1
   print *, c2

   c3 = c2
   print *, c3

   c2 = container ( 'ABC', com1(11), 12, com1(13), com2(14), c_com2(15,16), c_com2(17,18), com3(19), com3(20) )
   print *, c2

   c1 = c2
   print *, c1

   c3 = c1
   print *, c3

   c3 = container (c11 = com1(21), c32 = com3(30), c22 = c_com2(25,26) ,c31 = com3(29) , c12 = com1(23), i = 22, c1 = 'IBM',  cc21 = c_com2(27,28), c21 =  com2(24)  )
   print *, c3

   c2 = c3%return()
   print *, c2

   c1 = return(c2)
   print *, c1

end program
