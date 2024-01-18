! GB DTP extension using:
! ftcx_dtp -ql -qnodefaultpv -qdeferredlp -qreuse=self -qreuse=base /tstdev/F2003/generic/operator/functional/genericOperatorRecursive002.f
! opt variations: -qnol -qdefaultpv -qnodeferredlp -qreuse=none

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

   type base(n1,k1)    ! (20,4)
      integer, kind              :: k1
      integer, len               :: n1
      integer(k1)                :: i
      class(base(:,k1)), pointer :: next => null()
      contains
         procedure, pass :: badd
         generic :: operator(+) => badd
   end type

   type, extends(base):: child    ! (20,4)
      integer(k1) :: j
   end type

   contains

   recursive function badd ( a, b )
      class(base(*,4)), intent(in) :: a
      class(base(*,4)), intent(in)   :: b
      class(base(:,4)), allocatable :: badd


      select type ( a )
         type is ( base(*,4) )
            allocate ( base(20,4) :: badd )
            badd%i = a%i + b%i
         type is ( child(*,4) )
            allocate ( child(20,4) :: badd )
            select type ( badd )
               type is ( child(*,4) )
                  badd%i = a%i + b%i
                  select type ( b )
                     type is ( child(*,4) )
                        badd%j = a%j + b%j
                  end select
            end select
      end select

      if ( associated ( b%next ) .and. associated ( a%next ) ) then
         allocate ( badd%next, source = a%next + b%next )
      end if

   end function

end module


program genericOperatorRecursive002
   use m

   class(base(:,4)), pointer :: b1
   class(base(:,4)), allocatable, target :: b2, b3

   class(base(:,4)), pointer :: tmp => null()

   allocate ( b1, source = base(20,4)( 100, null() ) )
   tmp => b1

   do i = 1, 9
      allocate ( tmp%next, source = base(20,4)((i+1)*100, null() ) )
      tmp => tmp%next
   end do

   allocate ( b2, source  = base(20,4)( 1000, null() ) )
   tmp => b2

   do i = 1, 9
      allocate ( tmp%next, source = base(20,4)((i+1)*1000, null() ) )
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
   allocate ( b2, source = base(20,4)(10, null() ) + base(20,4)(20, null() ) )
   tmp => b2
   do while ( associated ( tmp ) )
      print *, tmp%i
      tmp => tmp%next
   end do

   deallocate ( b1, b2, b3 )

   allocate ( b1, source = child(20,4) ( 10, null(), 100 ) )
   tmp => b1
   do i = 1, 5
      allocate ( tmp%next, source = child(20,4)((i+1)*10, null(),(i+1)*100) )
      tmp => tmp%next
      allocate ( tmp%next, source = base(20,4) ((i+2)*1000) )
      tmp => tmp%next
   end do

   allocate ( b2, source = child(20,4) ( 100, null(), 1000 ) )
   tmp => b2
   do i = 1, 5
      allocate ( tmp%next, source = child(20,4)((i+1)*100, null(),(i+1)*1000) )
      tmp => tmp%next
      allocate ( tmp%next, source = base(20,4) ((i+1)*10000) )
      tmp => tmp%next
   end do

   print *, 'Linked List 4'
   allocate ( b3, source = b1 + b2 )
   tmp => b3
   do while ( associated ( tmp ) )
      select type ( tmp )
         type is ( base(*,4) )
            print *, tmp%i
         type is ( child(*,4) )
            print *, tmp%i, tmp%j
      end select
      tmp => tmp%next
   end do

end program
