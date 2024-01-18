!* =================================================================== &
!*
!* DATE                       : March 2, 2011
!* ORIGIN                     : AIX Compiler Development,
!*
!* PRIMARY FUNCTIONS TESTED   : Argument Presence Enhancement
!*
!* DESCRIPTION                : Testing proper functionality of
!*                              argument presence
!*                              as elemental procedures
!* EXCLUDED DUE TO INTERP: 391032
!*
!234567890123456789012345678901234567890123456789012345678901234567890

 integer, pointer :: p1(:), p2(:)
 integer, target :: t1(2)
 integer, allocatable :: a1(:), a2(:)
 logical :: ret(2)

 p2 => t1
 allocate (a2(1))
 nullify(p1)

 call sub1(p1,p2,a1,a2, ret)
 if ( ANY(.NOT. ret) ) then
   STOP 1
 endif
 if ( ANY(.NOT. func1(p1,p2,a1,a2)) ) then
   STOP 2
 endif

 contains
 elemental subroutine sub1(w,x,y,z, ret)
 integer, intent(in), optional :: w, x, y, z
 logical, intent(out) :: ret
 ret = .TRUE.
 if ( PRESENT(w) .OR. .NOT. PRESENT(x) .OR. &
    & PRESENT(y) .OR. .NOT. PRESENT(z) ) then
    ret = .FALSE.
 endif
 end subroutine

 elemental logical function func1(w,x,y,z)
 integer, intent(in), optional :: w, x, y, z
 func1 = .TRUE.
 if ( PRESENT(w) .OR. .NOT. PRESENT(x) .OR. &
    & PRESENT(y) .OR. .NOT. PRESENT(z) ) then
    func1 = .FALSE.
 endif
 end function
 end
