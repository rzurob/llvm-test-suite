! GB DTP extension using:
! ftcx_dtp -qk -qreuse=base /tstdev/F2003/mxminAll/con/maxMin/mxminArrayArgObj1.f
! opt variations: -qck -qnok -qreuse=none

!#######################################################################
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*  ===================================================================
!*
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : William Zhang 
!*  DATE                       : 1/05/2006
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   : Section 13.7.71[3,4,6,8,9]:
!*                               character argument for MAX/MIN intrinsics 
!*                             :
!*  SECONDARY FUNCTIONS TESTED : 
!*
!*
!*  DESCRIPTION                : MAX/MIN with named constant as actual
!*                               argument to struct constructor.
!*                               Also test while max/min used inside
!*                               select type construct
!* ===================================================================

  program mxminArrayArgObj1 

     type base(k1,n1)    ! (4,3)
         integer, kind :: k1
         integer, len  :: n1
         character(n1) :: bname(2,3) 
     end type

     type, extends(base) :: child    ! (4,3)
          character(n1) :: cname(2,3)
     end type

     type, extends(child) :: parent    ! (4,3) 
          character(n1) :: pname(2,3)
     end type
    

     type(base(4,3))   :: bdt
     type(child(4,3))  :: cdt    
     type(parent(4,3)) :: pdt

     character*3 x(2,3), y(2,3)

     parameter(x = "abc", y = "xyz")

     bdt = base(4,3)(max(x, y))     

     cdt = child(4,3)(base = bdt , cname = min(x, y)) 

     pdt = parent(4,3)(child = cdt, pname = max(min(x,y), max(x,y)))

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
               type is (base(4,*))
                  error stop 4_4
               type is (parent(4,*))
                  if(any(max(arg%pname, v) .ne. "zzz")) then
                        error stop 5_4
                  endif
               class default
                  error stop 6_4
            end select
                   
         end subroutine

  end program mxminArrayArgObj1 

