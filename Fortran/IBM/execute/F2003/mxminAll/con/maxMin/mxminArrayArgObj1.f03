!*  ===================================================================
!*
!*  DATE                       : 1/05/2006
!*
!*  PRIMARY FUNCTIONS TESTED   : Section 13.7.71[3,4,6,8,9]:
!*                               character argument for MAX/MIN intrinsics
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : MAX/MIN with named constant as actual
!*                               argument to struct constructor.
!*                               Also test while max/min used inside
!*                               select type construct
!* ===================================================================

  program mxminArrayArgObj1

     type base
         character*3 :: bname(2,3)
     end type

     type, extends(base) :: child
          character*3 :: cname(2,3)
     end type

     type, extends(child) :: parent
          character*3 :: pname(2,3)
     end type

     type(base)   :: bdt
     type(child)  :: cdt
     type(parent) :: pdt

     character*3 x(2,3), y(2,3)

     parameter(x = "abc", y = "xyz")

     bdt = base(max(x, y))

     cdt = child(base = bdt , cname = min(x, y))

     pdt = parent(child = cdt, pname = max(min(x,y), max(x,y)))

     if(any(bdt%bname .ne. "xyz")) then
            error stop 1_4
     endif

     if(any(cdt%cname .ne. "abc")) then
            error stop 2_4
     endif

     if(any(pdt%pname .ne. "xyz")) then
            error stop 3_4
     endif

     call sub(pdt)

     contains

         subroutine sub(arg)
            class(*), intent(in) :: arg
            character*3 v(2,3)
            v = "zzz"

            select type (arg)
               type is (base)
                  error stop 4_4
               type is (parent)
                  if(any(max(arg%pname, v) .ne. "zzz")) then
                        error stop 5_4
                  endif
               class default
                  error stop 6_4
            end select

         end subroutine

  end program mxminArrayArgObj1

