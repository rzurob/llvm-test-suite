!#######################################################################
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*  ===================================================================
!*
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : William Zhang 
!*  DATE                       : 2/05/2006
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   : Section 13.7.71[3,4,6,8,9]:
!*                               character argument for MAX*/MIN* intrinsics 
!*                             :
!*  SECONDARY FUNCTIONS TESTED : 
!*
!*
!*  DESCRIPTION                : MAXLOC/MINLOC with variable as actual
!*                               argument to struct constructor.
!* ===================================================================

  program mxminlocVarArgObj1 

     type base
         integer :: bname
     end type

     type, extends(base) :: child
          integer :: cname(2)
     end type

     type, extends(child) :: parent 
          integer :: pname(3)
     end type
    
     type(base)   :: bdt
     type(child)  :: cdt    
     type(parent) :: pdt

    integer v(3)
    logical :: m(2,3) = .true.
    character*3 :: x(5) = (/(char(i+70), i = 1,10,2)/)
    character*3 :: y(2,3) = reshape((/"bbb", "aaa", "ccc", "ddd","fff", "ggg"/),(/2,3/))

     m(1,2) = .false.

     bdt = base(minloc(x,dim=1,mask=.true.))     

     cdt = child(base = bdt , cname = maxloc(y, dim=2))

     pdt = parent(child = cdt, pname = minloc(y, dim=1, mask=m))

     if(bdt%bname .ne. 1) error stop 1_4

     if(any(cdt%cname .ne. 3)) error stop 2_4

     v = pdt%pname 

     if(v(1) .ne. 2 .or. v(2) .ne. 2 .or. v(3) .ne. 1) error stop 3_4

     call sub(pdt)

     contains
         
         subroutine sub(arg)
            class(*), intent(in) :: arg
            integer v(3)
             
            select type (arg)
               type is (base)
                  error stop 4_4
               type is (parent)
                  if(maxval(arg%pname ) .ne. 2) error stop 5_4
               class default
                  error stop 6_4
            end select
                   
         end subroutine

  end program mxminlocVarArgObj1 

