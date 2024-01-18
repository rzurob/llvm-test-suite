!*  ============================================================================
!*
!*  DATE                       : 2011-01-17
!*
!*  PRIMARY FUNCTIONS TESTED   : Complex Part Designator
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REFERENCE                  : Feature Number 383634
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION
!*
!*  This program tests the complex part designator:
!*     Test the use in allocate and pointer statements both as scalar and array
!*     Test the use in equivalence statement as array
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

program main

   implicit none

   integer, parameter :: N = 100
   integer i

   real r
   complex, allocatable :: c
   complex, pointer :: p
   complex, target :: t

   complex, allocatable, dimension(:) :: c_a
   complex, pointer, dimension(:) :: p_a
   complex, target, dimension(N) :: t_a

   real, dimension(N) :: r_a
   complex, dimension(N) :: c_n

   EQUIVALENCE (c_n(N/2), r_a(N/2))        ! Valid
   EQUIVALENCE (c_n(N/2)%RE, r_a(N/2))     ! Invalid
   EQUIVALENCE (c_n(N/2)%IM, r_a(N/2))     ! Invalid

   ! Scalar
   allocate(c)                             ! Valid
   c%RE = 1.4567                           ! Valid
   c%IM = 0.4567                           ! Valid
   p => t                                  ! Valid
   p%RE = 1.4567                           ! Valid
   p%IM = 0.4567                           ! Valid

   ! Scalar
   allocate(c%RE)                          ! Invalid
   allocate(c%IM)                          ! Invalid
   p%RE => t                               ! Invalid
   p%IM => t                               ! Invalid
   deallocate(c%RE)                        ! Invalid
   deallocate(c%IM)                        ! Invalid

   ! Array
   allocate(c_a(N))                        ! Valid
   c_a%RE = 2.34567                        ! Valid
   c_a%IM = 0.34567                        ! Valid
   p_a => t_a                              ! Valid
   do i = 1, N
      p_a(i)%RE = c_a(i)%RE                ! Valid
      p_a(i)%IM = c_a(i)%IM                ! Valid
   end do

   ! Array
   allocate(c_a(N)%RE)                     ! Invalid
   allocate(c_a(N)%IM)                     ! Invalid
   do i = 1, N
      p_a(i)%RE => t_a(i)                  ! Invalid
      p_a(i)%IM => t_a(i)                  ! Invalid
   end do
   deallocate(c_a%RE)                      ! Invalid
   deallocate(c_a%IM)                      ! Invalid

end program main
