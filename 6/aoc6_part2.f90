program aoc6_part2
  use iso_fortran_env
  implicit none

  real(real64) :: time, distance, t1, t2

  time = 5.3837288e7
  distance = 3.33163512891532e14

  ! -t**2. + t0*t - d = 0
  ! t = (t0+-sqrt(t**2-4*d)/2

  t1 = (time+sqrt(time**2-4.*distance))/2.
  t2 = (time-sqrt(time**2-4.*distance))/2.

  print *, "answer", floor(t1-t2)

end program aoc6_part2
