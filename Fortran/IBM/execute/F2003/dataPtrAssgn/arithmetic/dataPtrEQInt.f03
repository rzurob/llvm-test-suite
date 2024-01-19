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
!* - data-target has bindC attr, defined by C routine
!* - lb of data-ptr is globale var defined by C routine
!*
!234567890123456789012345678901234567890123456789012345678901234567890

module m
   use ISO_C_BINDING

   type base
      integer(C_INT), pointer :: i_ptr(:)
   end type

   integer(C_INT), bind(c), target :: val(20)
   integer(C_INT), bind(c) :: lb

   interface
        subroutine csub(a) bind(c)
            use ISO_C_BINDING
            integer(C_INT) a
        end subroutine
   end interface

end module

program main

    use m

    type(base) :: b

    call csub(lb)

    b%i_ptr(lb:) =>  val(ubound(val,1):1:-2)

    if ( .not. associated(b%i_ptr, val(ubound(val,1):1:-2))) error stop 1
    if (lbound(b%i_ptr,1) /= 21 ) error stop 3
    if (ubound(b%i_ptr,1) /= 30 ) error stop 5

    if ( any ( b%i_ptr == (/( i, i=120,101,-2 ) /) .neqv. .TRUE. ) ) error stop 7

 End program

