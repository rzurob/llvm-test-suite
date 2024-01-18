!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : dataPtrNEInt.f
!*
!*  DATE                       : Aug 31, 2006
!*
!*  PRIMARY FUNCTIONS TESTED   : Pointer Assignment Enhancement
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION
!*
!* - data-targets are components of a bindC derived-type
!* - data-ptr/targets' kind type parameter is name constant from ISO_C_BINDING module
!* - lb/ub of data-ptr is the named constant from ISO_C_BINDING module
!*
!234567890123456789012345678901234567890123456789012345678901234567890

module m
   use ISO_C_BINDING

   type, bind(c) :: A
      character(C_CHAR) :: c_tar(10)
      integer(C_INT) :: i_tar(10)
   end type

   type base
      character(:), pointer :: c_ptr(:)
      integer(C_INT), pointer :: i_ptr(:)
      type(A), allocatable ::  aa
   end type

end module

program main

    use ISO_C_BINDING, ONLY : C_CHAR,  C_INT
    use m

    type(base), target :: bb

    allocate(bb%aa)

    bb%aa%i_tar  = (/ ( i+64, i = 1,10 ) /)
    bb%aa%c_tar  = (/ (achar(bb%aa%i_tar(i)), i=1,10) /)

    bb%aa  = A((/ (achar(bb%aa%i_tar(i)), i=1,10) /), bb%aa%i_tar)


    bb%i_ptr(bb%aa%i_tar(1):) => bb%aa%i_tar(::2)

    if ( .not. associated(bb%i_ptr, bb%aa%i_tar(::2))) stop 2
    if (lbound(bb%i_ptr,1) /= 65 ) stop 3
    if (ubound(bb%i_ptr,1) /= 69 ) stop 4
    if ( any ( bb%i_ptr .ne. (/65,67,69,71,73/) ) ) stop 5

    bb%c_ptr(C_CHAR:C_INT) => bb%aa%c_tar(10:1:-2)

    if ( .not. associated(bb%C_ptr)) stop 6
    if (lbound(bb%c_ptr,1) /= 1 ) stop 7
    if (ubound(bb%c_ptr,1) /= 4 ) stop 8
    if ( .not. all ( bb%c_ptr .eq. (/ 'J', 'H', 'F', 'D' /)))  stop 9

 End program

