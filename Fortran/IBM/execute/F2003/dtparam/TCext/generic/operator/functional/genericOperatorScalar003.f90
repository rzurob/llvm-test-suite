! GB DTP extension using:
! ftcx_dtp -qck -ql -qdeferredlp /tstdev/F2003/generic/operator/functional/genericOperatorScalar003.f
! opt variations: -qnock -qnol -qnodeferredlp

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
!*  SECONDARY FUNCTIONS TESTED : with Operator( )
!*
!*  DRIVER STANZA              : xlf95
!*
!*  DESCRIPTION                : Binary Operator: Scalar to Scalar (**,//)
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

   type pow(n1,k1)    ! (20,4)
      integer, kind :: k1
      integer, len  :: n1
      integer(k1)   :: i
   end type

   type base(n2,k2)    ! (20,4)
      integer, kind :: k2
      integer, len  :: n2
      integer(k2)   :: i = -999
      contains
         procedure, pass :: powwpow
         procedure, pass :: powwbase
         procedure, pass :: powwint
         generic :: operator ( ** ) => powwpow, powwbase, powwint
   end type

   type, extends(base) :: child(k3,n3)    ! (20,4,1,3)
      integer, kind             :: k3
      integer, len              :: n3
      character(kind=k3,len=n3) :: c = 'xxx'
      contains
         generic :: operator ( // ) => concatwchild, concatwchar
         procedure, pass :: concatwchild
         procedure, pass :: concatwchar
   end type

   contains

      type(base(20,4)) function powwpow ( a, b )
         class(base(*,4)), intent(in) :: a
         class(pow(*,4)), intent(in)  :: b

         powwpow%i = a%i ** b%i
         print *, 'powwpow'

      end function

      type(base(20,4)) function powwbase ( a, b )
         class(base(*,4)), intent(in) :: a
         class(base(*,4)), intent(in)  :: b

         powwbase%i = a%i ** b%i
         print *, 'powwbase'

      end function

      type(base(20,4)) function powwint ( a, b )
         class(base(*,4)), intent(in) :: a
         integer, intent(in)  :: b

         powwint%i = a%i ** b
         print *, 'powwint'

      end function

      type(child(20,4,1,3)) function concatwchild ( a, b )
         class(child(*,4,1,*)), intent(in)  :: a
         class(child(*,4,1,*)), intent(in)  :: b

         concatwchild%i = a%i
         concatwchild%c = a%c(1:1) // b%c(2:3)
         print *, 'concatwchild'

      end function

      type(child(20,4,1,3)) function concatwchar ( a, b )
         class(child(*,4,1,*)), intent(in)   :: a
         character(3), intent(in)  :: b

         concatwchar%i = a%i
         concatwchar%c = a%c(1:1) // b(2:3)
         print *, 'concatwchar'

      end function

end module

program genericOperatorScalar003
   use m

   type(pow(20,4)) :: p0 = pow(20,4)(0)

   class(base(:,4)), pointer  :: b1, b2, b3
   class(child(:,4,1,:)), pointer :: c1, c2

   allocate ( b1, source = base(20,4)  ( 2 ) )
   allocate ( b2, source = child(20,4,1,3) ( 3, 'ABC' ) )

   allocate ( c1, source = child(20,4,1,3) ( 4, 'abc') )
   allocate ( c2, source = child(20,4,1,3) ( 5, 'def') )

   allocate ( b3, source = b1 ** b2 )
   if ( b3%i /= 8 ) error stop 1_4

   nullify ( b3 )

   allocate ( b3, source = b1 ** p0 )
   if ( b3%i /= 1 ) error stop 2_4

   nullify ( b3 )

   allocate ( b3, source = b2 ** 3 )
   if ( b3%i /= 27 ) error stop 3_4

   nullify ( b3 )

   allocate ( b3, source = ( b1 ** b2 ) ** c1 )
   if ( b3%i /= 4096 ) error stop 4_4

   allocate ( c1, source = ( c1 // c2 ) )
   if ( ( c1%i /= 4 ) .or. ( c1%c /= 'aef' ) ) error stop 5_4

   allocate ( c2, source = c1 // 'xBC' )
   if ( ( c2%i /= 4 ) .or. ( c2%c /= 'aBC' ) ) error stop 6_4

   select type ( g => b2 )
      type is ( child(*,4,1,*) )
         allocate ( c2, source = ( c1 // g ) )
   end select

   if ( ( c2%i /= 4 ) .or. ( c2%c /= 'aBC' ) ) error stop 7_4

end program
