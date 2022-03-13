# ---------
#  6-5
# ---------
# ---------------------------------------------------------- #

# �x�stomato �o��data.frame�J�w�L
save(tomato, file = "data/tomato.rdata")

# �q�O���鲾��tomato
rm(tomato)

# �d��tomate�O�_�٦s�b
head(tomato)

# �qrdataŪ��
load("data/tomato.rdata")

# �d�ݥ��{�b�O�_�s�b
head(tomato)

# ---------------------------------------------------------- #

# �إߤ@�Ǫ���
n <- 20
r <- 1:10
w <- data.frame(n, r)

# �ɬd����
n
r
w

# �x�s����
save(n, r, w, file = "data/multiple.rdata")

# �R������
rm(n, r, w)

# ���̮����F��?
n
r
w

# �⥦�̦AŪ���i��
load("data/multiple.rdata")

# �A���d�ݥ��̪��s�b
n
r
w

# ---------------------------------------------------------- #

#  �إߤ@�Ӫ���
smallVector <- c(1, 5, 4)

# �˵���
smallVector

# �x�s��rds�ɡ�
saveRDS(smallVector, file='thisObject.rds')
  
# Ū���ɮסA�����x�s��t�@�Ӫ���
thatVect <- readRDS('thisObject.rds')

# ��ܥ�
thatVect

# �ˬd���̬O�_�ۦP
identical(smallVector, thatVect)

# ---------------------------------------------------------- #