! GB DTP extension using:
! ftcx_dtp -qk -qdeferredlp -qreuse=base /tstdev/F2003/mxminAll/misc/mxminMisc14.f
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
!*  DESCRIPTION                : MAX*/MIN* with class obj.
!*                                
!* ===================================================================

module misc14 

   type base(k1,n1)    ! (4,3)  
     integer, kind :: k1
     integer, len  :: n1
     character(n1)    bname
   end type

   type, extends (base) :: child    ! (4,3)
     character(n1) :: cname 
   end type

   class(child(4,:)), allocatable :: charC(:)


end module misc14 

program mxminMisc14  
use misc14 

   character*3 x, y

   x = 'fff'
   y = 'aaa'

   allocate(charC(3), source=(/child(4,3)(min('aaa', 'zzz'), 'bbb'), child(4,3)('ccc', &
   'ddd'), child(4,3)('eee', max(x, y, x))/))

   if(MAXLOC(charC%bname, dim=1, mask = .true.) .ne. 3) then
        error stop 1_4
   endif

   if(MAXVAL(charC%bname, Dim = 1, Mask = .true.) .ne. 'eee')then
         error stop 2_4
   endif

   deallocate(charC)

 
end program mxminMisc14 


