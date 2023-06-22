library(rvest)
library(tidyverse)
library(RSelenium)
library(dplyr)
library(ggplot2)
library(ggrepel)
library(gridExtra)
library(jsonlite)
library(httr)

#############################################
#docker 이용해서 정보 테이블 만들기
remDr <- remoteDriver(remoteServerAddr = 'localhost',
                      port = 4412,
                      browserName = "chrome")

remDr$open() #remDr$close()

remDr$navigate("https://finance.naver.com/sise/sise_group_detail.naver?type=theme&no=128")

remDr$screenshot(display = TRUE)

webElem <-
  remDr$findElements(using = "css", value = "input[checked]")

for (i in 1:length(webElem))
  webElem[[i]]$clickElement()
#그냥 초기 값들을 선택해서 제거해도됨
remDr$screenshot(display = TRUE)

option <- paste("#option", 1:27, sep = "")

#항목 다시선택
for (i in option[c(4, 10, 15, 16, 12, 27)]) {
  webElem <- remDr$findElement(using = "css", i)
  
  webElem$clickElement()
}
remDr$screenshot(display = TRUE)


#새로 선택한 항목 적용해 주가 정보 테이블 얻기
element <- remDr$findElement(using = "css", "div.item_btn > a")
element$clickElement()

# 주가 정보 테이블 스크래핑
html <- remDr$getPageSource()[[1]]
table <- html %>%
  read_html() %>%
  html_table(fill = TRUE) %>%
  .[[3]] # 주가 정보 테이블은 3번째 테이블에 위치합니다

# 테이블 데이터 확인
head(table)

table <- table[-c(1, 35, 36), -2] %>%
  select(-토론실)

# save
enter_folder <- paste0(getwd(), "/enter_dt")
if (!dir.exists(enter_folder))
  dir.create(enter_folder)

write_csv(table, paste0(enter_folder, "/enter.csv"))

#############################################
# 테이블 불러오기
enter <- read_csv("enter_dt/enter.csv")

##### 1.유보율
enter$유보율 <- gsub(",", "", enter$유보율)
enter$유보율 <- as.numeric(enter$유보율)
top_a <- head(enter[order(-enter$유보율),], 7)


# 유보율 분포 시각화
ggplot(top_a, aes(
  x = 종목명,
  y = 유보율,
  label = paste(종목명, 유보율)
)) +
  geom_text_repel() +
  geom_jitter(width = 0.1,
              height = 0,
              color = "pink") +
  labs(title = "유보율 분포", x = "", y = "유보율") +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())

#유보율과 비슷한 지표들 상관 분석
correlation <- cor(enter[, c("시가총액", "유보율", "자산총계")])
print(correlation)

#유보율과 비슷한 지표들 시각화
###시가총액
enter$시가총액 <- as.numeric(enter$시가총액)
top_b <- head(enter[order(-enter$시가총액),], 7)

plot1 <-
  ggplot(top_b, aes(
    x = 종목명,
    y = 시가총액,
    label = sprintf("%s %.1f", 종목명, 시가총액)
  )) +
  geom_text_repel() +
  geom_jitter(width = 0.1,
              height = 0,
              color = "orange") +
  labs(title = "시가총액 분포", x = "", y = "시가총액") +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())

###자산총계
enter$자산총계 <- as.numeric(enter$자산총계)
top_c <- head(enter[order(-enter$자산총계),], 7)

plot2 <-
  ggplot(top_c, aes(
    x = 종목명,
    y = 자산총계,
    label = sprintf("%s %.1f", 종목명, 자산총계)
  )) +
  geom_text_repel() +
  geom_jitter(width = 0.1,
              height = 0,
              color = "purple") +
  labs(title = "자산총계 분포", x = "", y = "자산총계") +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())

# 그래프 결합
grid.arrange(plot1, plot2)

# 순서 한눈에 모아보기
number <- cbind(top_a$종목명 ,top_c$종목명, top_d$종목명)
colnames(number) <- c("유보율", "시가총액", "자본총액")
print(number)



##### 2. ROE
enter$ROE <- as.numeric(enter$ROE)
top_0 <- head(enter[order(-enter$ROE),], 20)

# ROE 분포 시각화
ggplot(top_0, aes(
  x = 종목명,
  y = ROE,
  label = sprintf("%s %.1f", 종목명, ROE)
)) +
  geom_text_repel() +
  geom_jitter(width = 0.1,
              height = 0,
              color = "skyblue") +
  labs(title = "ROE 분포", x = "", y = "ROE") +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())



##### 3. ROE와 유보율 (성장률)
enter$성장률 <- enter$ROE * enter$유보율
top_00 <- enter[order(-enter$성장률),][-1,]
print(top_00)

#성장률 순서정렬
top_1 <- enter[order(-enter$성장률), ][-c(1, 2), ] #1등 카카오 제거

# 성장률 분포 시각화
ggplot(top_1, aes(x = 성장률, y = ROE, label = 종목명)) +
  geom_text_repel() +
  geom_jitter(width = 0.1, height = 0, color = "BLUE") +
  labs(title = "카카오 제외 기업 성장률", x = "성장률", y = "ROE") +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())



##### 4. 외국인비율
#카카오, 하이브, 에스엠 4년 월별 데이터 비교


###KKO
url <-
  "https://api.finance.naver.com/siseJson.naver?symbol=035720&requestType=1&startTime=20201001&endTime=20230602&timeframe=month"
# Read JSON data
response <- GET(url)
content <- content(response, "text", encoding = "UTF-8")
contents <- strsplit(gsub("\\[|\\]|\n", "", content), ",\\s*")

#data.frame 만들기
dt_kko_m <-
  as.data.frame(matrix(contents[[1]], ncol = 7, byrow = TRUE))

#s의 1행을 제거 후 열이름 수정
dt_kko_m <- dt_kko_m[-1,]
names(dt_kko_m) <-
  c("날짜", "시가", "고가", "저가", "종가", "거래량", "외국인소진율")

dt_kko_m$날짜 <- gsub("\\\\|\"", "", dt_kko_m$날짜)
dt_kko_m$종가 <- as.numeric(dt_kko_m$종가)

dt_kko_m #카카오 4년 월별데이터


###SM
url <-
  "https://api.finance.naver.com/siseJson.naver?symbol=041510&requestType=1&startTime=20201001&endTime=20230602&timeframe=month"
# Read JSON data
response <- GET(url)
content <- content(response, "text", encoding = "UTF-8")
contents <- strsplit(gsub("\\[|\\]|\n", "", content), ",\\s*")

#data.frame 만들기
dt_sm_m <-
  as.data.frame(matrix(contents[[1]], ncol = 7, byrow = TRUE))

#s의 1행을 제거 후 열이름 수정
dt_sm_m <- dt_sm_m[-1,]
names(dt_sm_m) <-
  c("날짜", "시가", "고가", "저가", "종가", "거래량", "외국인소진율")

dt_sm_m$날짜 <- gsub("\\\\|\"", "", dt_sm_m$날짜)
dt_sm_m$종가 <- as.numeric(dt_sm_m$종가)


###하이브
url <-
  "https://api.finance.naver.com/siseJson.naver?symbol=352820&requestType=1&startTime=20201001&endTime=20230602&timeframe=month"
# Read JSON data
response <- GET(url)
content <- content(response, "text", encoding = "UTF-8")
contents <- strsplit(gsub("\\[|\\]|\n", "", content), ",\\s*")

#data.frame 만들기
dt_hi_m <-
  as.data.frame(matrix(contents[[1]], ncol = 7, byrow = TRUE))

#s의 1행을 제거 후 열이름 수정
dt_hi_m <- dt_hi_m[-1,]
names(dt_hi_m) <-
  c("날짜", "시가", "고가", "저가", "종가", "거래량", "외국인소진율")

dt_hi_m$날짜 <- gsub("\\\\|\"", "", dt_hi_m$날짜)
dt_hi_m$종가 <- as.numeric(dt_hi_m$종가)

dt_hi_m$외국인소진율 <- as.numeric(dt_hi_m$외국인소진율)
dt_kko_m$외국인소진율 <- as.numeric(dt_kko_m$외국인소진율)
dt_sm_m$외국인소진율 <- as.numeric(dt_sm_m$외국인소진율)

# 데이터 이름 설정
names <- c("하이브", "에스엠", "카카오")

# boxplot 그리기
boxplot(dt_hi_m$외국인소진율, dt_sm_m$외국인소진율, dt_kko_m$외국인소진율, names = names)
#하이브, 에스엠, 카카오 외국인소진율 4년 월별데이터 분석
#카카오가 외국인소진율 높다

#카카오가 외국인 비율 가장 높다
#그렇다면 이 비율이 종가에 영향을 미치나?

#카카오 상관분석
cor(dt_kko_m$외국인소진율, dt_kko_m$종가)

# 데이터프레임 생성
df <- data.frame(외국인소진율 = dt_kko_m$외국인소진율, 종가 = dt_kko_m$종가)

# 산점도 그래프 생성
ggplot(df, aes(x = 외국인소진율, y = 종가)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(x = "외국인소진율", y = "종가") +
  ggtitle("카카오 외국인소진율과 종가의 상관관계")


#상관관계 0.6687497이며, 그래프 그려본 결과 우상향하는 모양
#외국인소진율이 종가에 +영향 미친다고 볼 수 있다.
