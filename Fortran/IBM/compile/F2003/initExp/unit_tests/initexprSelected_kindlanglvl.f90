!* ===================================================================
!* XL FORTRAN TEST CASE                          IBM INTERNAL USE ONLY
!* ===================================================================
!* TEST CASE TITLE            : Initialization expression
!*
!* PROGRAMMER                 : Kelvin Li
!* DATE                       : March 31, 2006
!* ORIGIN                     : XL Compiler Development, Toronto Lab
!*
!* PRIMARY FUNCTIONS TESTED   : SELECTED_INT_KIND and SELECTED_REAL_KIND
!*                              intrinsics
!*
!* DESCRIPTION                : langlvl message
!* ===================================================================

@PROCESS INTSIZE(2)
subroutine sub1()
integer :: i=selected_int_kind(9)
integer :: j=selected_real_kind(6,70)
end subroutine

@PROCESS INTSIZE(4)
subroutine sub2()
integer :: i=selected_int_kind(9)
integer :: j=selected_real_kind(6,70)
end subroutine

@PROCESS INTSIZE(8)
subroutine sub3()
integer :: i=selected_int_kind(9)
integer :: j=selected_real_kind(6,70)
end subroutine
