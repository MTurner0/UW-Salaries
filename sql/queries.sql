select
    Campus, count(*) as Count
from
    Employees
group by
    Campus
order by
    Count desc;

select
    Campus, count(*) as Count, avg(Total_Pay) as Mean
from
    Employees
where
    Title like "%Professor%" and Dept_Description like "%Economics%"
group by
    Campus
order by
    Count desc;

select
    Dept_Description, min(Total_Pay) as Min, median(Total_Pay) over (partition by Dept_Description) as Median, avg(Total_Pay) as Mean, max(Total_Pay) as Max
from
    Employees
where
    Campus = "UW Madison" and Title like "%Professor%" and (Dept_Description like "%Economics%" or Dept_Description like "%Statistics%")
group by
    Dept_Description;