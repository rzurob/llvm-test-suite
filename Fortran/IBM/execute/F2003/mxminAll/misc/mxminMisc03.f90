!*  ===================================================================
!*
!*  DATE                       : 1/05/2006
!*
!*  PRIMARY FUNCTIONS TESTED   : Section 13.7.71[3,4,6,8,9]:
!*                               character argument for MAX/MIN intrinsics
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : MAX/MIN as actual argument passed to
!*                               subprogram with variable  and literal
!*                               as MAX/MIN argument. Forcus on
!*                               escape character
!*
!* ===================================================================

program mxminMisc03

   interface
       subroutine sub1(arg)
         character*1 arg
       end subroutine sub1
       function fun1(arg)
         character*1 arg, fun1
       end function
   end interface

   character*1 a, b, c, d, e, f, g, h, i, ver
   a = '\b'
   b = '\f'
   c = '\n'
   d = '\t'
   e = '\0'
   f = '\''
   g = '\"'
   h = '\\'
   i = '\x'

   if(ichar(max('\b','\f','\n','\t','\0','\'','\"','\\','\x')) .ne. 120) then
       error stop 1_4
   endif

   if(ichar(max(a,b,c,d,e,f,g,h,i)) .ne. 120) then
        error stop 2_4
   endif

   call sub1(max(a,b,c,d,e,f,g,h,i))

   ver = fun1(max('\b','\f','\n','\t','\0','\'','\"','\\','\x'))
   if( ver .ne. 'x')then
       error stop 4_4
   endif

   if(ichar(min('\b','\f','\n','\t','\0','\'','\"','\\','\x')) .ne. 0) then
       error stop 5_4
   endif

   if(ichar(fun1(min(a,b,c,d,e,f,g,h,i))) .ne. 0) then
        error stop 6_4
   endif

end program mxminMisc03

   subroutine sub1(arg)
     character*1 arg
     if(ichar(arg) .ne. 120) then
         error stop 3_4
     endif
   end subroutine sub1

   function fun1(arg)
     character*1 arg, fun1
     fun1 = arg
   end function fun1
