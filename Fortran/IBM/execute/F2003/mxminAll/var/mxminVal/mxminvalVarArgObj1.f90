!#######################################################################
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*  ===================================================================
!*
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : William Zhang 
!*  DATE                       : 1/15/2006
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   : Section 13.7.71[3,4,6,8,9]:
!*                               character argument for MAX/MIN intrinsics 
!*                             :
!*  SECONDARY FUNCTIONS TESTED : 
!*
!*
!*  DESCRIPTION                : MAXVAL/MINVAL as argument to struct 
!*                               constructor.
!*                               Also test while maxval/minval used inside
!*                               select type construct
!* ===================================================================

  program mxminvalVarArgObj1 

     type base
         character*3 :: bname
     end type

     type, extends(base) :: child
          character*3 :: cname(2)
     end type

     type, extends(child) :: parent 
          character*3 :: pname(3)
     end type
    
     type(base)   :: bdt
     type(child)  :: cdt    
     type(parent) :: pdt

     character*3 x(2,3)

     x = "abc"
     x(1,2) = "xyz"

     bdt = base(minval(x))     

     cdt = child(base = bdt , cname = maxval(x, dim=2)) 

     pdt = parent(child = cdt, pname = minval(x, dim=1, mask=.true.)) 

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
               type is (base)
                  error stop 4_4
               type is (parent)
                  if(maxval(arg%pname) .ne. "abc") then
                     error stop 5_4
                  end if
               class default
                  error stop 6_4
            end select
                   
         end subroutine

  end program mxminvalVarArgObj1 
