!*  ===================================================================
!*
!*  DATE                       : 1/15/2006
!*
!*  PRIMARY FUNCTIONS TESTED   : Section 13.7.71[3,4,6,8,9]:
!*                               character argument for MAX/MIN intrinsics
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : MAX/MIN with literal as actual
!*                               argument to struct constructor.
!*                               Also test while max/min used inside
!*                               select type construct
!* ===================================================================

  program mxminLiteralArrArgObj1

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

     bdt = base(max(reshape((/"abc","abc","abc","abc","abc", "abc"/), (/2,3/)), reshape((/"xyz","xyz","xyz","xyz", "xyz", "xyz"/), (/2,3/))))

     cdt = child(base = bdt , cname = min(reshape((/"abc","abc","abc","abc","abc","abc"/), (/2,3/)), reshape((/"xyz","xyz","xyz","xyz","xyz","xyz"/), (/2,3/))))

     pdt = parent(child = cdt, pname = max(min(reshape((/"abc","abc","abc","abc","abc","abc"/), (/2,3/)), reshape((/"xyz","xyz","xyz","xyz","xyz","xyz"/), (/2,3/))), max(reshape((/"abc","abc","abc","abc","abc","abc"/), (/2,3/)),reshape((/"xyz","xyz","xyz","xyz","xyz","xyz"/), (/2,3/)))))

     if(bdt%bname(1,2)(1:2) .ne. "xy") error stop 1_4

     if(any(bdt%bname .ne. "xyz")) error stop 2_4

     if(any(cdt%cname .ne. "abc")) error stop 3_4

     if(any(pdt%pname .ne. "xyz")) error stop 4_4

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

  end program mxminLiteralArrArgObj1

