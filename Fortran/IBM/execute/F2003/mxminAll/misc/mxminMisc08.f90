!*  ===================================================================
!*
!*  DATE                       : 1/05/2006
!*
!*  PRIMARY FUNCTIONS TESTED   : Section 13.7.71[3,4,6,8,9]:
!*                               character argument for MAX/MIN intrinsics
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : MAX/MIN as selector in associate construction
!*
!* ===================================================================

program mxminMisc08

   character*3  a(9), b(9), c(9)

   data (a(i), i=1, 5) /5*'gta'/, (a(i), i=6, 9) /4*'zyy'/
   data (b(i), i=1, 9) /9*'zza'/
   data (c(i), i=1, 9) /9*'zzz'/

   associate( As1 => max(a, b) )
        if( any(As1 .ne. 'zza')) error stop 1
   end associate

   associate( As2 => max(a, b, c) )
        if( any(As2 .ne. 'zzz')) error stop 2
   end associate

   associate( As3 => maxval(a) )
        if(As3 .ne. 'zyy') error stop 3
   end associate

   associate( As4 => maxloc(a) )
        if ( any(As4  .ne. 6)) error stop 4
   end associate

end program mxminMisc08
