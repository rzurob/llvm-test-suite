!*  ===================================================================
!*
!*  DATE                       : 11/01/2005
!*
!*  PRIMARY FUNCTIONS TESTED   : Section 4.5.4: Generic Type Bound Procedure
!*  SECONDARY FUNCTIONS TESTED : with Operator
!*
!*  DESCRIPTION                : operator: function being recursive and assigning polymorphic linked lists
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

   type base
      integer :: i
      class(base), pointer :: next => null()
      contains
         procedure, pass :: badd
         generic :: operator(+) => badd
   end type

   type, extends(base):: child
      integer :: j
      contains
         procedure, pass :: badd => cadd
   end type

   contains

   recursive function badd ( a, b )
      class(base), intent(in) :: a
      class(base), intent(in) :: b
      class(base), allocatable :: badd

      allocate ( base :: badd )
      badd%i = a%i + b%i

      if ( associated ( b%next ) .and. associated ( a%next ) ) then
         allocate ( badd%next, source = a%next + b%next )
      end if

   end function

   recursive function cadd ( a, b )
      class(child), intent(in) :: a
      class(base), intent(in)   :: b
      class(base), allocatable :: cadd

      allocate ( child :: cadd )
      cadd%i = a%i + b%i

      select type ( b )
         type is ( child )
            select type ( cadd )
               type is ( child )
                  cadd%j = a%j + b%j
            end select
      end select

      if ( associated ( b%next ) .and. associated ( a%next ) ) then
         allocate ( cadd%next, source = a%next + b%next )
      end if

   end function

end module

program genericOperatorRecursive003
   use m

   class(base), pointer :: b1
   class(base), allocatable, target :: b2, b3

   class(base), pointer :: tmp => null()

   allocate ( b1, source = base( 100, null() ) )
   tmp => b1

   do i = 1, 9
      allocate ( tmp%next, source = base((i+1)*100, null() ) )
      tmp => tmp%next
   end do

   allocate ( b2, source  = base( 1000, null() ) )
   tmp => b2

   do i = 1, 9
      allocate ( tmp%next, source = base((i+1)*1000, null() ) )
      tmp => tmp%next
   end do

   print *, 'Linked List 1'
   allocate ( b3, source  = b1 + b2 )
   tmp => b3
   do while ( associated ( tmp ) )
      print *, tmp%i
      tmp => tmp%next
   end do

   print *, 'Linked List 2'
   deallocate ( b1 )
   allocate ( b1, source = b2 + b3 )
   tmp => b1
   do while ( associated ( tmp ) )
      print *, tmp%i
      tmp => tmp%next
   end do

   print *, 'Linked List 3'
   deallocate ( b2 )
   allocate ( b2, source = base(10, null() ) + base(20, null() ) )
   tmp => b2
   do while ( associated ( tmp ) )
      print *, tmp%i
      tmp => tmp%next
   end do

   deallocate ( b1, b2, b3 )

   allocate ( b1, source = child ( 10, null(), 100 ) )
   tmp => b1
   do i = 1, 5
      allocate ( tmp%next, source = child((i+1)*10, null(),(i+1)*100) )
      tmp => tmp%next
      allocate ( tmp%next, source = base ((i+2)*1000) )
      tmp => tmp%next
   end do

   allocate ( b2, source = child ( 100, null(), 1000 ) )
   tmp => b2
   do i = 1, 5
      allocate ( tmp%next, source = child((i+1)*100, null(),(i+1)*1000) )
      tmp => tmp%next
      allocate ( tmp%next, source = base ((i+1)*10000) )
      tmp => tmp%next
   end do

   print *, 'Linked List 4'
   allocate ( b3, source = b1 + b2 )
   tmp => b3
   do while ( associated ( tmp ) )
      select type ( tmp )
         type is ( base )
            print *, tmp%i
         type is ( child )
            print *, tmp%i, tmp%j
      end select
      tmp => tmp%next
   end do

end program
