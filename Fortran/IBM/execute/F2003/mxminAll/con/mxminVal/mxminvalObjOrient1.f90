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
!*                               character argument for MAX*/MIN* intrinsics 
!*                             :
!*  SECONDARY FUNCTIONS TESTED : 
!*
!*
!*  DESCRIPTION                : MAXVAL/MINVAL with named constant as actual
!*                               argument to struct constructor.
!*                               Also test while max/min used inside
!*                               select type construct
!* ===================================================================

  program mxminvalObjOrient1 

     type base
         character*3 :: bname
     end type

     type, extends(base) :: child
          character*3 :: cname(2)
     end type

     type, extends(child) :: parent 
          character*3 :: pname(2)
     end type
    

     type(base)   :: bdt
     type(child)  :: cdt    
     type(parent) :: pdt

     character*3 x(2,3), y(2,3)

     parameter(x = "abc", y = "xyz")

     bdt = base(maxval(x))     

     cdt = child(base = bdt , cname = minval(y, dim=2)) 

     pdt = parent(child = cdt, pname = maxval(y, dim=2, mask=.true.))

     if(bdt%bname .ne. "abc") then
            error stop 1_4
     endif

     if(any(cdt%cname .ne. "xyz")) then
            error stop 2_4
     endif

     if(any(pdt%pname .ne. "xyz")) then
            error stop 3_4
     endif

     call sub(bdt)

     contains
         
         subroutine sub(arg)
            class(*), intent(in) :: arg
            character*3 v(2,3)
            v = "zzz"
 
            select type (arg)
               type is (base)
                  if(any(max(arg%bname, v) .ne. "zzz")) then
                        error stop 5_4
                  endif
               type is (parent)
                  error stop 4_4
               class default
                  error stop 6_4
            end select
                   
         end subroutine

  end program mxminvalObjOrient1 
