       ! 
       ! Use a generic/specific intrinsic procedure as iface-name
       ! c1215 procptr can not have an elemental attr
       ! 
       !procedure(abs), pointer :: pp1
       interface abs
         function abs(r)
         end function
       end interface
       procedure(abs), pointer :: pp1
       integer iii, jjj
       !pp1 => abs 
       iii = pp1(jjj)
       end
