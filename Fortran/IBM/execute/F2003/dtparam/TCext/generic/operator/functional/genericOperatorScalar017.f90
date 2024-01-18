! GB DTP extension using:
! ftcx_dtp -ql -qdeferredlp -qreuse=base /tstdev/F2003/generic/operator/functional/genericOperatorScalar017.f
! opt variations: -qnol -qnodeferredlp -qreuse=none

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
!*  DESCRIPTION                : Operator: type bound procedure in base and generic defined in
!*                                         child type,
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
      integer(k1)   :: i
      contains
         procedure :: add
         procedure, private :: addwitharray !<- only be used in type child
         generic :: operator(+) => add
   end type

   type, extends(base) :: child    ! (20,4)
      integer(k1) :: j
      contains
         generic :: operator(+) => addwitharray
   end type

   contains

   class(base(:,4)) function add ( a, b )
      allocatable :: add
      class(base(*,4)), intent(in) :: a, b

      allocate ( add, source= base(20,4) ( i = a%i + b%i ) )
   end function

   class(base(:,4)) function addwitharray ( a, b )
      allocatable :: addwitharray(:)
      class(base(*,4)), intent(in) :: a, b(:)


      allocate ( child(20,4) :: addwitharray(size(b)) )

      do i=1, size(b)
         select type ( a )
            type is ( child(*,4) )
               select type ( b )
                  type is ( child(*,4) )
                     addwitharray(i)%i  =  a%i + b(i)%i
                     select type ( addwitharray )
                        type is ( child(*,4) )
                           addwitharray(i)%j  =  a%j + b(i)%j
                        class default
                           error stop 1_4
                     end select
                  class default
                     error stop 2_4
               end select
            class default
               error stop 3_4
         end select
      end do

   end function

end module

program genericOperatorScalar017
   use m

   class(base(:,4)), allocatable :: b1, b2(:),b3(:)

   type(child(20,4)) :: c1, c2(2), c3(2)
   class(child(:,4)), pointer :: c11, c12(:), c13(:)

   allocate ( b1, source = child(20,4) ( 10, 20 ) )
   allocate ( b2(3), source = (/ (child(20,4) (i, i+10), i = 1, 3) /) )
   select type ( b1 )
      type is ( child(*,4) )
         allocate ( b3(3), source = ( b1 + b2 ) )
         print *, b3%i
         select type ( b3 )
            type is ( child(*,4) )
               print *, b3%j
         end select
      class default
         error stop 4_4
   end select

   c1 = child(20,4) ( 1, 2 )
   c2 = (/ child(20,4) ( 10, 11 ), child(20,4) ( 20, 21 ) /)

   select type ( g => c1 + c2 )
      type is ( child(*,4) )
         c3 = g
         print *, c3%i
         print *, c3%j
   end select

   allocate ( c11, source = child(20,4) ( 3,4 ) )
   allocate ( c12(4), source = (/ (child(20,4) (i,i*2),i= 11,14) /) )

   select type ( g => c11 + c12 )
      type is ( child(*,4) )
         allocate ( c13(4), source = g )
   end select

   print *, c13%i
   print *, c13%j

end program
