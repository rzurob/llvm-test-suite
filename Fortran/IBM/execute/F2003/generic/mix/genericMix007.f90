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
!*  SECONDARY FUNCTIONS TESTED : Mix generic type bounds
!*
!*  DRIVER STANZA              : xlf2003
!*
!*  DESCRIPTION                : with data structures
!*
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

   type element
      integer :: i
      class(element), pointer :: next => null()
      contains

         procedure :: add_ele_ele
         procedure :: sub_ele_ele
         procedure :: lt_ele_ele

         generic :: operator(+) => add_ele_ele
         generic :: operator(-) => sub_ele_ele
         generic :: operator(<) => lt_ele_ele

   end type

   type, extends(element) :: superelement
      integer :: j
   end type

   type linkedlist
      class(element), pointer :: head
      class(element), pointer :: tail
      contains

         procedure :: add_ll_ll
         procedure :: add_ll_ele
         generic :: operator(+) => add_ll_ele, add_ll_ll

         procedure :: sub_ll_ll
         procedure :: sub_ll_ele
         generic :: operator(-) => sub_ll_ll, sub_ll_ele

         procedure :: lt_ll_ll
         procedure :: lt_ll_ele
         generic :: operator ( < ) => lt_ll_ll, lt_ll_ele

         procedure :: appendele
         procedure :: appendint
         generic :: append => appendele, appendint

   end type

   contains

      subroutine appendele ( ll, e )
         class(linkedlist), intent(inout) :: ll
         class(element), intent(in) :: e
         
         class(element), pointer :: a
         
         allocate ( a, source = e )

         if ( .not. associated ( ll%head ) )  then
            ll%head => a
         else
            ll%tail%next => a
         end if

         ll%tail => a

      end subroutine

      subroutine appendint ( ll, i )
         class(linkedlist), intent(inout) :: ll
         integer, intent(in) :: i

         class(element), pointer :: a

         allocate ( a, source = element(i=i) )

         if ( .not. associated ( ll%head ) )  then
            ll%head => a
         else
            ll%tail%next => a
         end if

         ll%tail => a

      end subroutine


      logical function lt_ll_ele( a, b )

         class(linkedlist), intent(in) :: a
         class(element), intent(in) :: b

         class(element), pointer :: tmp, tmpresult

         lt_ll_ele = .true.

         tmp => a%head

         do while ( associated( tmp ) )
            lt_ll_ele = lt_ll_ele .and. ( tmp%i < b%i )
            tmp => tmp%next
         end do

      end function

      logical function lt_ll_ll( a, b )

         class(linkedlist), intent(in) :: a
         class(linkedlist), intent(in) :: b

         class(element), pointer :: tmpa, tmpb

         lt_ll_ll = .true.

         tmpa => a%head
         tmpb => b%head

         do while ( associated( tmpa ) )
            lt_ll_ll = lt_ll_ll .and. ( tmpa%i < tmpb%i )
            tmpa => tmpa%next
            tmpb => tmpb%next
         end do

      end function

      class(linkedlist) function sub_ll_ele( a, b )
         allocatable :: sub_ll_ele

         class(linkedlist), intent(in) :: a
         class(element), intent(in) :: b

         class(element), pointer :: tmp, tmpresult

         allocate ( sub_ll_ele, source = a )

         tmp => a%head
         tmpresult => sub_ll_ele%head

         do while ( associated( tmp ) )
            tmpresult%i = tmp%i - b%i
            tmp => tmp%next
            tmpresult  => tmpresult%next
         end do

      end function

      class(linkedlist) function sub_ll_ll( a, b )
         allocatable :: sub_ll_ll

         class(linkedlist), intent(in) :: a
         class(linkedlist), intent(in) :: b

         class(element), pointer :: tmp, tmpresult

         allocate ( sub_ll_ll, source = a )

         tmp => b%head
         tmpresult => sub_ll_ll%head

         do while ( associated( tmp ) )

            tmpresult%i = tmpresult%i - tmp%i

            tmp => tmp%next
            tmpresult  => tmpresult%next
         end do

      end function

      class(linkedlist) function add_ll_ele( a, b )
         allocatable :: add_ll_ele

         class(linkedlist), intent(in) :: a
         class(element), intent(in) :: b

         class(element), pointer ::  tmpresult,tmp
         allocate( add_ll_ele, source =a )

         tmp => a%head
         tmpresult => add_ll_ele%head

         do while ( associated( tmp ) )
            tmpresult%i = tmp%i + b%i
            tmp => tmp%next
            tmpresult  => tmpresult%next
         end do

      end function

      class(linkedlist) function add_ll_ll( a, b )
         allocatable :: add_ll_ll

         class(linkedlist), intent(in) :: a
         class(linkedlist), intent(in) :: b

         class(element), pointer :: tmp, tmpresult

         allocate ( add_ll_ll, source = a )

         tmp => b%head
         tmpresult => add_ll_ll%head

         do while ( associated( tmp ) )
            tmpresult%i = tmpresult%i + tmp%i
            tmp => tmp%next
            tmpresult  => tmpresult%next
         end do

      end function

      class(element) function add_ele_ele( a, b )
         allocatable :: add_ele_ele

         class(element), intent(in) :: a
         class(element), intent(in) :: b

         select type ( a )
            type is ( element )
               allocate ( add_ele_ele, source = element ( i = a%i + b%i ) )
            type is ( superelement )
               select type ( b )
                  type is ( element )
                     allocate ( add_ele_ele, source = element ( i = a%i + b%i ) )
                  type is ( superelement )
                     allocate ( add_ele_ele, source = superelement ( i = a%i + b%i, j = a%j + b%j ) )
               end select
         end select

      end function

      class(element) function sub_ele_ele( a, b )
         allocatable :: sub_ele_ele

         class(element), intent(in) :: a
         class(element), intent(in) :: b

         select type ( b )
            type is ( element )
               allocate ( sub_ele_ele, source = a + element( i = -1 * b%i ) )
            type is ( superelement )
               allocate ( sub_ele_ele, source = a + superelement( i = -1 * b%i, j = -1 * b%j )  )
         end select

      end function

      logical function lt_ele_ele( a, b )
         class(element), intent(in) :: a
         class(element), intent(in) :: b

         select type ( a )
            type is ( element )
               lt_ele_ele = ( a%i < b%i )
            type is ( superelement )
               select type ( b )
                  type is ( element )
                     lt_ele_ele = ( a%i < b%i )
                  type is ( superelement )
                     lt_ele_ele = ( a%i < b%i ) .and. ( a%j < b%j )
               end select
         end select

      end function

end module

program genericMix007
   use m

   type(linkedlist) :: a, b, c
   pointer :: b
   class(element), pointer :: tmp
   
   nullify (a%head, c%head)

   call a%append(1)
   call a%append( element(i=2) )
   call a%append( superelement(i=3,j=4) )
   
   call c%append(1)
   call c%append( element(i=2) )
   call c%append( superelement(i=3,j=4) )
   
   tmp => a%head
   
   do while(associated(tmp) )
      print *, tmp%i
      tmp=>tmp%next
   end do
  
   allocate( b, source = a + element(1) )

   tmp => b%head
   
   do while(associated(tmp) )
      print *, tmp%i
      tmp=>tmp%next
   end do
   	
   if ( .not. c < b  ) error stop 1_4

   allocate ( b, source = a + a )  

   tmp => b%head
   
   do while(associated(tmp) )
      print *, tmp%i
      tmp=>tmp%next
   end do

   allocate( b, source = a - element(3) )
   	
   tmp => b%head
   
   do while(associated(tmp) )
      print *, tmp%i
      tmp=>tmp%next
   end do

   allocate( b, source = a - c )
   	
   tmp => b%head
   
   do while(associated(tmp) )
      print *, tmp%i
      tmp=>tmp%next
   end do

end program
