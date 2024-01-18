! GB DTP extension using:
! ftcx_dtp -ql -qreuse=base /tstdev/F2003/mxminAll/var/mxminLoc/mxminlocVarArgObj1.f
! opt variations: -qnol -qreuse=none

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

     type base(n1,k1)    ! (20,4)
         integer, kind :: k1
         integer, len  :: n1
         integer(k1)   :: bname
     end type

     type, extends(base) :: child    ! (20,4)
          integer(k1) :: cname(2)
     end type

     type, extends(child) :: parent    ! (20,4) 
          integer(k1) :: pname(3)
     end type
    
     type(base(20,4))   :: bdt
     type(child(20,4))  :: cdt    
     type(parent(20,4)) :: pdt

    integer v(3)
    logical :: m(2,3) = .true.
    character*3 :: x(5) = (/(char(i+70), i = 1,10,2)/)
    character*3 :: y(2,3) = reshape((/"bbb", "aaa", "ccc", "ddd","fff", "ggg"/),(/2,3/))

     m(1,2) = .false.

     bdt = base(20,4)(minloc(x,dim=1,mask=.true.))     

     cdt = child(20,4)(base = bdt , cname = maxloc(y, dim=2))

     pdt = parent(20,4)(child = cdt, pname = minloc(y, dim=1, mask=m))

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
               type is (base(*,4))
                  error stop 4_4
               type is (parent(*,4))
                  if(maxval(arg%pname ) .ne. 2) error stop 5_4
               class default
                  error stop 6_4
            end select
                   
         end subroutine

  end program mxminlocVarArgObj1 

