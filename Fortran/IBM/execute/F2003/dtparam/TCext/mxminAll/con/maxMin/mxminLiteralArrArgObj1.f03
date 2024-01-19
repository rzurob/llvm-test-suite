! GB DTP extension using:
! ftcx_dtp -qck -qreuse=base /tstdev/F2003/mxminAll/con/maxMin/mxminLiteralArrArgObj1.f
! opt variations: -qnock -qreuse=none

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

     type base(k1,n1)    ! (1,3)
         integer, kind             :: k1
         integer, len              :: n1
         character(kind=k1,len=n1) :: bname(2,3)
     end type

     type, extends(base) :: child    ! (1,3)
          character(kind=k1,len=n1) :: cname(2,3)
     end type

     type, extends(child) :: parent    ! (1,3)
          character(kind=k1,len=n1) :: pname(2,3)
     end type

     type(base(1,3))   :: bdt
     type(child(1,3))  :: cdt
     type(parent(1,3)) :: pdt

     bdt = base(1,3)(max(reshape((/"abc","abc","abc","abc","abc", "abc"/), (/2,3/)), reshape((/"xyz","xyz","xyz","xyz", "xyz", "xyz"/), (/2,3/))))

     cdt = child(1,3)(base = bdt , cname = min(reshape((/"abc","abc","abc","abc","abc","abc"/), (/2,3/)), reshape((/"xyz","xyz","xyz","xyz","xyz","xyz"/), (/2,3/))))

     pdt = parent(1,3)(child = cdt, pname = max(min(reshape((/"abc","abc","abc","abc","abc","abc"/), (/2,3/)), reshape((/"xyz","xyz","xyz","xyz","xyz","xyz"/), (/2,3/))), max(reshape((/"abc","abc","abc","abc","abc","abc"/), (/2,3/)),reshape((/"xyz","xyz","xyz","xyz","xyz","xyz"/), (/2,3/)))))

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
               type is (base(1,*))
                  error stop 4_4
               type is (parent(1,*))
                  if(any(max(arg%pname, v) .ne. "zzz")) then
                        error stop 5_4
                  endif
               class default
                  error stop 6_4
            end select

         end subroutine

  end program mxminLiteralArrArgObj1

