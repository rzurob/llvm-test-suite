!* =================================================================== &
!*
!* DATE                       : March 2, 2011
!*
!* PRIMARY FUNCTIONS TESTED   : Argument Presence Enhancement
!*
!* DESCRIPTION                : Testing proper functionality of
!*                              argument presence
!*                              with differing intrinsic types
!*
!234567890123456789012345678901234567890123456789012345678901234567890

 integer, pointer :: ip1(:), ip2(:)
 integer, target :: it1(2)
 integer, allocatable :: ia1(:), ia2(:)

 complex, pointer :: cp1(:), cp2(:)
 complex, target :: ct1(2)
 complex, allocatable :: ca1(:), ca2(:)

 real, pointer :: rp1(:), rp2(:)
 real, target :: rt1(2)
 real, allocatable :: ra1(:), ra2(:)

 character(:), pointer :: chp1(:), chp2(:)
 character(9), target :: cht1(2)
 character(:), allocatable :: cha1(:), cha2(:)

 ip2 => it1
 allocate (ia2(1))
 nullify(ip1)

 cp2 => ct1
 allocate (ca2(1))
 nullify(cp1)

 rp2 => rt1
 allocate (ra2(1))
 nullify(rp1)

 chp2 => cht1
 allocate (cha2(1),source=cht1(1))
 nullify(chp1)

 call isub1(ip1,ip2,ia1,ia2)
 it1 = ifunc1(ip1,ip2,ia1,ia2)

 call csub1(cp1,cp2,ca1,ca2)
 ct1 = cfunc1(cp1,cp2,ca1,ca2)

 call rsub1(rp1,rp2,ra1,ra2)
 rt1 = rfunc1(rp1,rp2,ra1,ra2)

 call chsub1(chp1,chp2,cha1,cha2)
 cht1 = chfunc1(chp1,chp2,cha1,cha2)
 contains
   subroutine isub1(w,x,y,z)
     integer, optional :: w(:), x(:), y(:), z(:)
     print *, PRESENT(w), PRESENT(x), PRESENT(y), PRESENT(z)
   end subroutine
   integer function ifunc1(w,x,y,z)
     integer, optional :: w(:), x(:), y(:), z(:)
     ifunc1 = 1
     print *, PRESENT(w), PRESENT(x), PRESENT(y), PRESENT(z)
   end function
   subroutine rsub1(w,x,y,z)
     real, optional :: w(:), x(:), y(:), z(:)
     print *, PRESENT(w), PRESENT(x), PRESENT(y), PRESENT(z)
   end subroutine
   real function rfunc1(w,x,y,z)
     real, optional :: w(:), x(:), y(:), z(:)
     rfunc1 = 1
     print *, PRESENT(w), PRESENT(x), PRESENT(y), PRESENT(z)
   end function
   subroutine csub1(w,x,y,z)
     complex, optional :: w(:), x(:), y(:), z(:)
     print *, PRESENT(w), PRESENT(x), PRESENT(y), PRESENT(z)
   end subroutine
   complex function cfunc1(w,x,y,z)
     complex, optional :: w(:), x(:), y(:), z(:)
     cfunc1 = (1,1)
     print *, PRESENT(w), PRESENT(x), PRESENT(y), PRESENT(z)
   end function
   subroutine chsub1(w,x,y,z)
     character(*), optional :: w(:), x(:), y(:), z(:)
     print *, PRESENT(w), PRESENT(x), PRESENT(y), PRESENT(z)
   end subroutine
   character(9) function chfunc1(w,x,y,z)
     character(*), optional :: w(:), x(:), y(:), z(:)
     chfunc1 = 'ldskjf'
     print *, PRESENT(w), PRESENT(x), PRESENT(y), PRESENT(z)
   end function
 end
