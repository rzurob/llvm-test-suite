! GB DTP extension using:
! ftcx_dtp -qk -qreuse=base /tstdev/F2003/mxminAll/var/maxMin/mxminVarArrArgObj1.f
! opt variations: -qck -qnok -qreuse=none

!*  ===================================================================
!*
!*  DATE                       : 1/15/2006
!*
!*  PRIMARY FUNCTIONS TESTED   : Section 13.7.71[3,4,6,8,9]:
!*                               character argument for MAX/MIN intrinsics
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : MAX/MIN with variable as actual
!*                               argument to struct constructor.
!*                               Also test while max/min used inside
!*                               select type construct
!* ===================================================================

  program mxminVarArrArgObj1

     type base(k1,n1)    ! (4,5)
         integer, kind :: k1
         integer, len  :: n1
         character(n1) :: bname(2,3)
     end type

     type, extends(base) :: child    ! (4,5)
          character(n1) :: cname(2,3)
     end type

     type, extends(child) :: parent    ! (4,5)
          character(n1) :: pname(2,3)
     end type

     type(base(4,5))   :: bdt
     type(child(4,5))  :: cdt
     type(parent(4,5)) :: pdt

     character*3 x(2,3)
     character*5 y(2,3)

     x = "abc"
     y = "xyzab"

     if(len(max(x, y)) .ne. 5 .or. len(min(x, y)) .ne. 5) then
          error stop 1_4
     endif

     bdt = base(4,5)(max(x, y))

     cdt = child(4,5)(base = bdt , cname = min(x, y))

     pdt = parent(4,5)(child = cdt, pname = max(min(x,y), max(x,y)))

     if(any(bdt%bname .ne. "xyzab")) then
            error stop 2_4
     endif

     if(any(cdt%cname .ne. "abc  ")) then
            error stop 3_4
     endif

     if(any(pdt%pname .ne. "xyzab")) then
            error stop 4_4
     endif

     call sub(pdt)

     contains

         subroutine sub(arg)
            class(*), intent(in) :: arg
            character*5 v(2,3)
            v = "zzzab"

            select type (arg)
               type is (base(4,*))
                  error stop 4_4
               type is (parent(4,*))
                  if(any(max(arg%pname, v) .ne. "zzzab")) then
                        error stop 5_4
                  endif
               class default
                  error stop 6_4
            end select

         end subroutine

  end program mxminVarArrArgObj1
