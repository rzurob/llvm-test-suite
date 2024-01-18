       ! 
       ! Use a specific intrinsic procedure as iface-name
       ! c1215: procptr can not have an elemental attr
       interface iabs
         function iabs(a)
         integer, intent(in) :: a
         end function
       end interface 
       procedure(iabs), pointer :: pp1
       integer iii, jjj
       pp1 => qabs 
       jjj = -19
       iii = pp1(jjj)
       if (iii .ne. 19) stop 10 
       end
