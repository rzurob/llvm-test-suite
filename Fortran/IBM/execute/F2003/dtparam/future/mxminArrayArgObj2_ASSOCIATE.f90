! GB DTP extension using:
! ftcx_dtp -qk -qdeferredlp -qreuse=base /tstdev/F2003/mxminAll/con/maxMin/mxminArrayArgObj2.f
! opt variations: -qck -qnok -qnodeferredlp -qreuse=none

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
!*  DESCRIPTION                : MAX/MIN with named constant as selector 
!*                               in associate construct 
!*                                
!*
!* ===================================================================

  program mxminArrayArgObj2 

   type base(k1,n1)    ! (4,3)
     integer, kind :: k1
     integer, len  :: n1
     character(n1)    bname(2,5)
   end type

   type, extends (base) :: child    ! (4,3)
     character(n1) :: cname(2,5)
   end type

   class(child(4,:)), allocatable :: z(:)

   character*3 x1(2,5), y1(2,5), x2(2,5), y2(2,5), x3(2,5), y3(2,5)

   parameter(x1 = 'aaa', y1 = 'bbb', x2 ="ccc", y2="ddd", x3="eee", &
       y3 = "fff")

   allocate(z(3), source=(/child(4,3)(max(x1, y1), y2), child(4,3)(x3, min(x2, y2)), &
          child(4,3)(max(x3, y3, x1),y2)/))

   associate(As1 => max(z(1)%cname, z(2)%bname))

       if(any(As1 .ne. "eee") )then
         error stop 1_4
       end if

   end associate

   associate(As2 => min(z(1)%cname, z(2)%bname, z(3)%cname))

       if(any(As2 .ne. "ddd")) then
         error stop 2_4
        endif

   end associate

   deallocate(z)

  end program mxminArrayArgObj2 

