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
!*  DESCRIPTION                : MAX*/MIN* as actual argument passed to
!*                               intrinsic function 
!*                               max*/min* itself as intrinisic function 
!*                                
!* ===================================================================

module misc25 

   character*6 charA(2,3), charB(2,3)
   integer v(3)

end module misc25

program mxminMisc25 
   use misc25

   charA = repeat('abcdef', 6)
   charB = reshape(source = (/'fBdesc', '______', '%%&&**', '093874', 'Zddedf' &
          ,'gSSkjd' /), shape = (/2,3/))

   if(MAXLOC(MAX(MINVAL(charA), MAXVAL(charB, dim=2, mask=charB .GT. 'cdfghy')), DIM=1, mask = .true.) .ne. 2) then
      error stop 1_4
   endif

 
end program mxminMisc25 


