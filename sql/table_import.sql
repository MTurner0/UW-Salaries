create table `Employees` (
    `Fiscal_Year` smallint not null,
    `Name` varchar(50) not null,
    `Title` varchar(30) not null,
    `Dept_Description` varchar(30) not null,
    `Campus` varchar(25) not null,
    `Start_Date` date not null,
    `Total_Pay` decimal not null
);

load data infile 'employees.tsv'
into table Employees
fields terminated by '\t' enclosed by '"'
lines terminated by '\n'
(Fiscal_Year, Name, Title, Dept_Description, Campus, @Start_Date, @Total_Pay, @Details)
set Start_Date = str_to_date(@Start_Date, '%m/%d/%Y'),
Total_Pay = cast(replace(replace(@Total_Pay, "$", ""), ",", "") as decimal);

create table `Instructors` (
    `id` int not null,
    `Name` varchar(50) not null,
    primary key (`id`)
);

load data infile 'instructors.csv'
into table Instructors
fields optionally enclosed by '"' terminated by ','
lines terminated by '\n'
ignore 1 rows
(id, Name);

create table `Courses` (
    `uuid` varchar(50) not null,
    `Name` varchar(150) not null,
    `Number` smallint not null,
    primary key (`uuid`)
)

load data infile 'courses.csv'
into table Courses
fields optionally enclosed by '"' terminated by ','
lines terminated by '\n'
ignore 1 rows
(uuid, Name, Number);

create table `Course_Offerings` (
    `uuid` varchar(50) not null,
    `course_uuid` varchar(50) not null,
    `term_code` smallint not null,
    `Name` varchar(150) not null,
    primary key (`uuid`)
);

load data infile 'course_offerings.csv'
into table Course_Offerings
fields optionally enclosed by '"' terminated by ','
lines terminated by '\n'
ignore 1 rows
(uuid, course_uuid, term_code, Name);

create table `Grade_Distributions` (
    `course_offering_uuid` varchar(50) not null,
    `section_number` tinyint not null,
    `A_count` smallint not null,
    `AB_count` smallint not null,
    `B_count` smallint not null,
    `BC_count` smallint not null,
    `C_count` smallint not null,
    `D_count` smallint not null,
    `F_count` smallint not null,
    `S_count` smallint not null,
    `U_count` smallint not null,
    `CR_count` smallint not null,
    `N_count` smallint not null,
    `P_count` smallint not null,
    `I_count` smallint not null,
    `NW_count` smallint not null,
    `NR_count` smallint not null,
    `Other_count` smallint not null,
    primary key (`course_offering_uuid`)
);

load data infile 'grade_distributions.csv'
into table Grade_Distributions
fields optionally enclosed by '"' terminated by ','
lines terminated by '\n'
ignore 1 rows
(course_offering_uuid, section_number, A_count, AB_count, B_count, BC_count, C_count, D_count, F_count, S_count, U_count, CR_count, N_count, P_count, I_count, NW_count, NR_count, Other_count);

create table `Rooms` (
    `uuid` varchar(50) not null,
    `facility_room` varchar(20) not null,
    `room_code` varchar(10)
    primary key (`uuid`)
);

load data infile 'rooms.csv'
into table Rooms
fields optionally enclosed by '"' terminated by ','
lines terminated by '\n'
ignore 1 rows
(uuid, facility_room, room_code);

create table `Schedules` (
    `uuid` varchar(50) not null,
    `start_time` smallint not null,
    `end_time` smallint not null,
    `Mon` boolean not null,
    `Tues` boolean not null,
    `Wed` boolean not null,
    `Thurs` boolean not null,
    `Fri` boolean not null,
    `Sat` boolean not null,
    `Sun` boolean not null,
    primary key (`uuid`)
);

load data infile 'schedules.csv'
into table Schedules
fields optionally enclosed by '"' terminated by ','
lines terminated by '\n'
ignore 1 rows
(uuid, start_time, end_time, Mon, Tues, Wed, Thurs, Fri, Sat, Sun);

create table `Sections` (
    `uuid` varchar(50) not null,
    `course_offering_uuid` varchar(50) not null,
    `section_type` varchar(10) not null,
    `number` smallint not null,
    `room_uuid` varchar(50),
    `schedule_uuid` varchar(50) not null,
    primary key (`uuid`)
);

load data infile 'sections.csv'
into table Sections
fields optionally enclosed by '"' terminated by ','
lines terminated by '\n'
ignore 1 rows
(uuid,course_offering_uuid,section_type,number,room_uuid,schedule_uuid);

create table `Subject_Memberships` (
    `subject_code` varchar(6) not null,
    `course_offering_uuid` varchar(50) not null,
    primary key (`course_offering_uuid`)
);

load data infile 'subject_memberships.csv'
into table Subject_Memberships
fields optionally enclosed by '"' terminated by ','
lines terminated by '\n'
ignore 1 rows
(subject_code, course_offering_uuid;

create table `Subjects` (
    `code` varchar(6) not null,
    `name` varchar(50) not null,
    `abbreviation` varchar(15) not null,
    primary key (`code`)
);

load data infile 'subjects.csv'
into table Subjects
fields optionally enclosed by '"' terminated by ','
lines terminated by '\n'
ignore 1 rows
(code, name, abbreviation);

create table `Teachings` (
    `instructor_id` int not null,
    `section_uuid` varchar(50) not null,
    primary key (`section_uuid`)
);

load data infile 'teachings.csv'
into table Teachings
fields optionally enclosed by '"' terminated by ','
lines terminated by '\n'
ignore 1 rows
(instructor_id, section_uuid);