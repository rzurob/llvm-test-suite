       ! 
       ! Use a specific intrinsic procedure as iface-name
       ! c1215 denotes that a proptr cannot have an elemental attr
       ! 
       !procedure(iabs), pointer :: pp1
       procedure(len), pointer :: pp1
       real iii, jjj
       iii = pp1(jjj)
       end
