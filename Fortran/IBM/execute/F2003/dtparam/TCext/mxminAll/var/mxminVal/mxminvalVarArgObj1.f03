! GB DTP extension using:
! ftcx_dtp -qk -qreuse=base /tstdev/F2003/mxminAll/var/mxminVal/mxminvalVarArgObj1.f
! opt variations: -qck -qnok -qreuse=none

!*  ===================================================================
!*
!*  DATE                       : 1/15/2006
!*
!*  PRIMARY FUNCTIONS TESTED   : Section 13.7.71[3,4,6,8,9]:
!*                               character argument for MAX/MIN intrinsics
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : MAXVAL/MINVAL as argument to struct
!*                               constructor.
!*                               Also test while maxval/minval used inside
!*                               select type construct
!* ===================================================================

  program mxminvalVarArgObj1

     type base(k1,n1)    ! (4,3)
         integer, kind :: k1
         integer, len  :: n1
         character(n1) :: bname
     end type

     type, extends(base) :: child    ! (4,3)
          character(n1) :: cname(2)
     end type

     type, extends(child) :: parent    ! (4,3)
          character(n1) :: pname(3)
     end type

     type(base(4,3))   :: bdt
     type(child(4,3))  :: cdt
     type(parent(4,3)) :: pdt

     character*3 x(2,3)

     x = "abc"
     x(1,2) = "xyz"

     bdt = base(4,3)(minval(x))

     cdt = child(4,3)(base = bdt , cname = maxval(x, dim=2))

     pdt = parent(4,3)(child = cdt, pname = minval(x, dim=1, mask=.true.))

     if(bdt%bname .ne. "abc") error stop 1_4

     if(any(cdt%cname(1:1) .ne. "xyz")) error stop 2_4

     if(any(cdt%cname(2:2) .ne. "abc")) error stop 3_4

     if(any(pdt%pname .ne. "abc")) error stop 4_4

     call sub(pdt)

     contains

         subroutine sub(arg)
            class(*), intent(in) :: arg
            character*3 v(2,3)
            v = "zzz"

            select type (arg)
               type is (base(4,*))
                  error stop 4_4
               type is (parent(4,*))
                  if(maxval(arg%pname) .ne. "abc") then
                     error stop 5_4
                  end if
               class default
                  error stop 6_4
            end select

         end subroutine

  end program mxminvalVarArgObj1
