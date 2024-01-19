! GB DTP extension using:
! ftcx_dtp -ql -qdeferredlp -qreuse=base /tstdev/F2003/generic/operator/functional/genericOperatorDummyArg005.f
! opt variations: -qnol -qnodeferredlp -qreuse=none

!*  ===================================================================
!*
!*  DATE                       : 11/01/2005
!*
!*  PRIMARY FUNCTIONS TESTED   : Section 4.5.4: Generic Type Bound Procedure
!*  SECONDARY FUNCTIONS TESTED : with operator(+)
!*
!*  DESCRIPTION                : operator: poly (assumed-shaped) array dummy arguments being the operand
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
      integer, kind :: k1
      integer, len  :: n1
      integer(k1)   :: i = -999
      contains
         procedure, pass :: badd
         generic :: operator(+) => badd
   end type

   type, extends(base) :: child    ! (20,4)
      integer(k1) :: j =-999
   end type

   contains

      integer function badd ( a, b )
         class(base(*,4)), intent(in) :: a
         class(base(*,4)), intent(in) :: b(:)

         allocatable :: badd(:)
         allocate ( badd(size(b)))

         do k = 1,size(b)
            badd(k) = b(k)%i + a%i
         end do

         select type ( a )
            type is ( child(*,4) )
               select type ( b )
                  type is ( child(*,4) )
                     do k = 1,size(b)
                        badd(k) = badd(k) + b(k)%j + a%j
                     end do
               end select
         end select

         print *, 'badd'

      end function

end module

program genericOperatorDummyArg005
   use m

   class(base(:,4)), allocatable :: b1, b2(:)
   integer, allocatable  :: i(:)

   allocate ( b2(3), source = (/ base(20,4)(10), base(20,4)(20), base(20,4)(30) /) )
   allocate ( b1, source = base(20,4)(1) )

   allocate ( i(size(b2)),source= add(b1,b2) )
   print *, i

   deallocate ( i )
   allocate ( i(size(b2)),source= add1(b1,b2) )
   print *, i

   deallocate ( i )
   allocate ( i(1))
   i = add (base(20,4)(100), (/ base(20,4)(110) /) )
   print *,i

   i = add1 ( child(20,4)(10,100), (/ child(20,4)(20,200) /)  )
   print *,i

   deallocate ( b1, b2 )

   allocate ( b1, source = child(20,4)(1,2))
   allocate ( b2(10), source = (/ (child(20,4)(j*10,j*100), j=1,10) /) )

   deallocate (i)
   allocate ( i(10) )

   i = add( b1, b2 )
   print *,i

   i = add( b1, (/ (child(20,4)(j*100,j*1000), j=1,10) /) )
   print *,i

   contains

      integer function add(a, b)
         class(base(*,4)), intent(in) :: a
         class(base(*,4)), intent(in)  :: b(:)
         dimension :: add(:)
         allocatable :: add

         print *, 'add'

         allocate (add(size(b)), source = a + b)

      end function

      function add1(a, b)
         class(base(*,4)), intent(in) :: a
         class(base(*,4)), intent(in)  :: b(-1:)
         integer, allocatable :: add1(:)

         print *, 'add1'
         allocate (add1(size(b)), source = a + b)

      end function

end program

