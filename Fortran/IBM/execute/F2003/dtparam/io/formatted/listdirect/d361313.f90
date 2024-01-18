!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : d361313.f
!*
!*  DATE                       : Jan. 24 2009
!*
!*  PRIMARY FUNCTIONS TESTED   : LIST-DIRECTED INTRINSIC IO
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REFERENCE                  :
!*
!*  DESCRIPTION
!*  Defect 361313
!234567890123456789012345678901234567890123456789012345678901234567890
module m
   type A
      integer      :: i1=-99
      character(3) :: c1(4)="***"
   end type

   type B(l2)
      integer,len  :: l2=3
      integer      :: i2(l2)=-99
      type(A)      :: a1comp
   end type

  type D(l4)
    integer,len  :: l4=2

    type(B(l4*2-1)) :: b1comp
  end type
end module

program d361313
   use m

   type(D),target  :: tar
   type(D(:)),pointer :: ptr=>null()

   ptr=>tar

   open(10,file='d361313.dat')

   print *,"**read ptr%b1comp**"

   read(10,*) ptr%b1comp

   print *,"output1:"
   print *,ptr%b1comp%i2
   print *,ptr%b1comp%a1comp%i1,ptr%b1comp%a1comp%c1
   print *,ptr%b1comp
   print *,ptr%b1comp%a1comp
   print *,ptr

   rewind 10

   print *,"**read ptr**"

   read(10,*) ptr

   print *,"output2:"
   print *,ptr%b1comp%i2
   print *,ptr%b1comp%a1comp%i1,ptr%b1comp%a1comp%c1
   print *,ptr%b1comp
   print *,ptr%b1comp%a1comp
   print *,ptr

   close(10)

end  program

