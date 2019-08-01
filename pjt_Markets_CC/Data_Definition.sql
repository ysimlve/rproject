/*
Server: USCHISQLDEV01.DelphiPrd.Am.JonesLangLaSalle.com\DEV2012 (EDW_UAT)
Database: [GDIMRegAPAC]
Schema: 
*/
USE [GDIMRegAPAC]

CREATE SCHEMA CN_CD_CC;

--DROP TABLE CN_CD_CC.Companies_Infor_51Job
--Primary Key --
--company,contact,city,industry,sub_industry,Is_Current--
CREATE TABLE CN_CD_CC.Companies_Infor_51Job
(
	ID INT PRIMARY KEY IDENTITY(100,1),
	company NVARCHAR(200) NOT NULL,
	[address] NVARCHAR(1000),
    [address1] NVARCHAR(1000),
	city NVARCHAR(100) NOT NULL,
	building NVARCHAR(200),
	contact NVARCHAR(200),
	official_contact NVARCHAR(10),
	offical_contacts_qualified NVARCHAR(10),
	coType NVARCHAR(100),
	coSize NVARCHAR(100),
	industry NVARCHAR(100),
	sub_industry NVARCHAR(100),
	is_local NVARCHAR(10),
	headquarter NVARCHAR(100),
	legal_person NVARCHAR(200),
	registered_No NVARCHAR(200),
	registered_city NVARCHAR(200),
	registered_office NVARCHAR(200),
	registered_capital NVARCHAR(200),
	establish_date NVARCHAR(200),
	business_end_date NVARCHAR(200),
	business_Sector_RA NVARCHAR(1000),
	op_status NVARCHAR(200),
	official_website NVARCHAR(500),
	web_record_code NVARCHAR(200),
	coDesc NVARCHAR(4000),
	jobCount_all NVARCHAR(100),
	jobCount_cd NVARCHAR(100),
	salaryRange_all NVARCHAR(200),
	salaryRange_cd NVARCHAR(200),
	job_issue_latest DATE NOT NULL,
	scape_date DATE NOT NULL,
	data_source NVARCHAR(100),
	recruitPage_51job NVARCHAR(500),
	 Effective_Date DATE NOT NULL,
	[Expiry_Date] DATE NOT NULL DEFAULT '9999-12-31',
	Is_Current SMALLINT NOT NULL DEFAULT 1
)

ALTER TABLE CN_CD_CC.Companies_Infor_51Job ADD Latitude NVARCHAR(50)
ALTER TABLE CN_CD_CC.Companies_Infor_51Job ADD Lontitude NVARCHAR(50)
ALTER TABLE CN_CD_CC.Companies_Infor_51Job ADD GoBDXin NVARCHAR(50)
ALTER TABLE CN_CD_CC.Companies_Infor_51Job ADD GoBDXin_Date DATE
ALTER TABLE CN_CD_CC.Companies_Infor_51Job ADD GoBDMap NVARCHAR(50)
ALTER TABLE CN_CD_CC.Companies_Infor_51Job ADD GoBDMap_DATE DATE


SELECT COUNT(1) FROM CN_CD_CC.Companies_Infor_51Job

SELECT TOP 1000 * FROM CN_CD_CC.Companies_Infor_51Job WHERE data_source = '11467' order by ID DESC
--GoBDXin Records
SELECT TOP 150 * FROM CN_CD_CC.Companies_Infor_51Job WHERE data_source = '51job' AND GoBDXin IS NULL order by ID DESC
SELECT TOP 150 ID,company FROM CN_CD_CC.Companies_Infor_51Job WHERE data_source = '51job' AND GoBDXin IS NULL order by ID DESC
--GoBDMap Records
SELECT TOP 1500 * FROM CN_CD_CC.Companies_Infor_51Job  WHERE Is_Current = 1 AND GoBDMap IS NULL AND data_source = '51job'  ORDER BY ID DESC

SELECT TOP 1000 * FROM CN_CD_CC.Companies_Infor_51Job WHERE Is_Current = 0

SELECT TOP 1000 * FROM CN_CD_CC.Companies_Infor_51Job where data_source = '51job' and scape_date >= '2017-04-05'

SELECT TOP 1000 * FROM CN_CD_CC.Companies_Infor_51Job where data_source = 'liepin' and scape_date >= '2017-03-28'

select * into CN_CD_CC.Companies_Infor_51Job_BK_20170405 FROM CN_CD_CC.Companies_Infor_51Job 
SELECT
            TOP 500 ID,company
          FROM CN_CD_CC.Companies_Infor_51Job
          WHERE 1 = 1
          AND Is_Current = 1
          AND offical_contacts_qualified = 'YES'
          AND LEN(address1) < 1
          ORDER BY ID DESC

select * from [CN_CD_CC].[Companies_Infor_51Job_BDXin_20170303] order by id desc
select * from CN_CD_CC.Companies_Infor_51Job where address1 = 'NotFound'


--DROP TABLE CN_CD_CC.Properties_Info
--Primary Key --
--Property,District,City,Is_Current--
CREATE TABLE CN_CD_CC.Properties_Info
(
	ID INT PRIMARY KEY IDENTITY(100,1),
	Property NVARCHAR(200),
	[Address] NVARCHAR(1000),
	District NVARCHAR(50),
	City NVARCHAR(20),
	City_Direction NVARCHAR(20),					  --所在方位 : 市中心
	Business_District NVARCHAR(50),				  --所在商圈：	天府新城商圈
	Ring_District NVARCHAR(50),                       --环线状况：	三环至绕城
	Latitude NVARCHAR(50),                              --纬度
	Lontitude NVARCHAR(50),                            --经度
	 
	PT_Type  NVARCHAR(20) DEFAULT '写字楼',   --物业类别
	PT_Grade NVARCHAR(20),                            --物业级别：	甲级
	Salse_Type NVARCHAR(20),                         --租售形式：	出租
	Deliver_Criterion NVARCHAR(20),				  --交房标准：	清水房
	Year_Built NVARCHAR(50),							  --建成时间：	2014年
	Area_Range NVARCHAR(50),                        --面积区间：	16000m2
	Refer_Price_Rent NVARCHAR(100),               --参考租金：	30元/m2.月
	PT_Intro NVARCHAR(MAX),                          --项目介绍
	Current_Customers NVARCHAR(MAX),          --入驻客户

	Floors NVARCHAR(100),                               --总楼层数：	29层
	Gross_Area NVARCHAR(100),                       --总建筑面积：	44483m2
	Std_Floor_Area NVARCHAR(100),                 --标准层面积：	1120m2
	Clear_Height NVARCHAR(100),                     --标准层高：	3米
	Cover_Area  NVARCHAR(100),                      --占地面积：	暂无资料
	Management_Fee NVARCHAR(100),               --物管费：	16元/m2.月
	Parking_Lot NVARCHAR(100),                        --停车位数量：	1878
	Parking_Fee_By_Month NVARCHAR(100),        --车位包月价：	600元/月
	Management_Company NVARCHAR(500),        --物业管理：	宇洲国际大酒店
	Developer NVARCHAR(500),                            --开发商：	成都复地置业有限公司
	Lift NVARCHAR(2000),                                   --电梯：	品牌电梯6部
	AC_System NVARCHAR(2000),                       --空调系统：	暂无资料
	COM_System NVARCHAR(2000),                     --通讯系统：	暂无资料
	ENG_System NVARCHAR(2000),                     --供电电源：	暂无资料
	POS_System NVARCHAR(2000),                     --消防系统：	暂无资料
	Security_System NVARCHAR(2000),               --安保系统：	暂无资料

	Reg_Date NVARCHAR(100),                            --登记时间： 2016年12月02日
	Visit_Count NVARCHAR(100),                         --点击次数： 1282
	Off028_Link NVARCHAR(500),                        --37th column
	scape_date DATE NOT NULL,
	data_source NVARCHAR(100),
	 Effective_Date DATE NOT NULL,
	[Expiry_Date] DATE NOT NULL DEFAULT '9999-12-31',
	Is_Current SMALLINT NOT NULL DEFAULT 1
)




/************************************************Data Delivery************************************************/
DROP VIEW vw_Company;
CREATE VIEW vw_Company AS
SELECT company,[building],[contact],[official_contact],[offical_contacts_qualified],[address],[Latitude],[Lontitude],[coType],[coSize],
			[industry],[sub_industry],[jobCount_all],[jobCount_cd],[salaryRange_all],[salaryRange_cd],
		    [legal_person],[registered_No],[registered_city],[registered_office],[registered_capital],
            [establish_date],[business_end_date],[business_Sector_RA],[op_status],[official_website],[coDesc]
FROM CN_CD_CC.Companies_Infor_51Job 
WHERE contact <> '' 
           AND official_contact = 'YES' 
	       AND offical_contacts_qualified = 'YES' 
		   AND Latitude is not null 
		   AND building is not null
		   AND legal_person <> ''
		   AND building <> ''
		   AND Is_Current = 1

SELECT * FROM [CN_CD_CC].[Properties_Info]

SELECT * FROM vw_Company 


CREATE VIEW vw_deliverble_company AS
SELECT ROW_NUMBER() OVER(PARTITION BY company ORDER BY building DESC,legal_person DESC) row_num,
			ID,company,[contact],[building], [address],[coType],[coSize],[sub_industry],
			[legal_person], [registered_city], [registered_office], [registered_capital],[official_website],
			[business_Sector_RA]
FROM CN_CD_CC.Companies_Infor_51Job 
WHERE 1 = 1
	        AND offical_contacts_qualified = 'YES' 
			AND Is_Current = 1

SELECT * FROM vw_deliverble_company WHERE row_num in (1,2)

CREATE VIEW vw_deliverble_company_final AS
SELECT ID,company,[contact],[building], [address],[coType],[coSize],
			CASE WHEN sec_industry IS NULL THEN [sub_industry]
					 WHEN sec_industry IS NOT NULL THEN [sub_industry] + ' & ' + sec_industry
			END AS Industry,
			[legal_person], [registered_city], [registered_office], [registered_capital],[business_Sector_RA],[official_website]
			
FROM(
	SELECT A.*,B.sub_industry sec_industry
	FROM (SELECT * FROM vw_deliverble_company WHERE row_num = 1) A
		LEFT JOIN (SELECT * FROM vw_deliverble_company WHERE row_num = 2) B
		ON A.company = B.company)C


SELECT * FROM vw_deliverble_company_final 

SELECT TOP 500 * FROM CN_CD_CC.Companies_Infor_51Job WHERE ID IN (SELECT ID FROM vw_deliverble_company_final ) AND LEN(address1) < 1 ORDER BY ID DESC


SELECT * FROM CN_CD_CC.Companies_Infor_51Job WHERE ID IN (SELECT ID FROM vw_deliverble_company_final ) AND Latitude IS NULL ORDER BY ID DESC



