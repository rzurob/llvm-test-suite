!*  ===================================================================
!*
!*  DATE                       : 1/05/2006
!*
!*  PRIMARY FUNCTIONS TESTED   : Section 13.7.71[3,4,6,8,9]:
!*                               character argument for MAX/MIN intrinsics
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : MAX*/MIN* as component in struct constructor
!*
!* ===================================================================

program mxminMisc16

     type base
         integer  id
     end type

     type, extends(base) :: child
          character*3 :: name
     end type

     type, extends(child) :: grandchild
          character*3 :: gname(3)
     end type

     character*4 :: x(5)

     type (child) :: c
     type (grandchild) :: g

     type (base) :: b

     x = "zzzz"

     x(2) = 'aaaa'

     b = base(minloc(x, dim=1, mask=.true.))

     c = child(base = b, name = max("ddd", "aaa"))

     g = grandchild(child = c, gname = minval(x))

     if(b%id .ne. 2) then
        error stop 1_4
     endif

     if(c%name .ne. 'ddd')then
        error stop 2_4
     endif

     if(any(g%gname .ne. "aaa")) then
        error stop 3_4
     endif

     g = grandchild(child = c, gname = maxval(pack(x, mask = .true.)))

     if(any(g%gname .ne. "zzz")) then
        error stop 4_4
     endif

end program mxminMisc16

