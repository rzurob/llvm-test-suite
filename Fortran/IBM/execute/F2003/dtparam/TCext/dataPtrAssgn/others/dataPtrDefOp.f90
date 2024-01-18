! GB DTP extension using:
! ftcx_dtp -ql -qdeferredlp /tstdev/F2003/dataPtrAssgn/others/dataPtrDefOp.f
! opt variations: -qnol -qnodeferredlp

!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Aug 31, 2006
!*
!*  PRIMARY FUNCTIONS TESTED   : Pointer Assignment Enhancement
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION
!*
!* - data-ptr is arg of the function for defined operator
!* - data-ptr is the function for defined operator
!*
!234567890123456789012345678901234567890123456789012345678901234567890

module m
   type base(n1,k1)    ! (20,4)
	integer, kind :: k1
	integer, len  :: n1
	integer(k1)      i
   end type
end module

module n
   use m
   interface operator (*)
       function multip(in1, in2)
	   import
           type(base(*,4)), intent(in) :: in1(:), in2(:,:)
           type(base(:,4)), pointer :: multip(:,:)
       end function
   end interface
end module

program main
    use n

    type(base(:,4)), pointer :: p1(:), p2(:,:)
    logical precision_r4

    allocate(p1(10), source =  (/(base(20,4)(i), i = 1,10 )/) )

    p2(1:3,1:3) => p1

    if ( .not. associated(p2)) stop 11
    if ( any (lbound(p2) .ne. (/1,1/) )) stop 12
    if ( any (ubound(p2) .ne. (/3,3/) )) stop 13

    p2 = p1*p2

    if ( any(p2%i .ne. reshape((/(i,i=10,2,-1)/), (/3,3/)))) stop 14
end

function multip(in1, in2)
   use m
   type(base(*,4)), target, intent(in) :: in1(:), in2(:,:)
   type(base(:,4)), pointer :: multip(:,:)

   multip(1:3,1:3) => in1(ubound(in1,1):lbound(in1,1):-1)

   if ( .not. associated(multip)) stop 21
   if ( any (lbound(multip) .ne. (/1,1/) )) stop 22
   if ( any (ubound(multip) .ne. (/3,3/) )) stop 23

end function
