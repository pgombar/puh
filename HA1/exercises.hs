-- exercise 1.1
concat3 s1 s2 s3 = s1 ++ (if length s2 < 2 then "" else s2) ++ s3

-- exercise 1.2
showSalary amount bonus =
  if amount < 0 then "Salary is negative!"
  else "Salary is " ++ show amount ++ (if bonus /= 0 then ", and a bonus " ++ show bonus else "")
