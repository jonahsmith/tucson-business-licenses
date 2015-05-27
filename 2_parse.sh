echo -e 'Accept_Date\tBusiness_Name\tStart_Date\tLicense_Type\tActivity_Code_1\tActivity_Code_2\tActivity_Code_3\tNAICS_Code\tOwner_Type\tStreet_Number\tStreet_Direction\tStreet_Name\tStreet_Suffix\tSuite/Apt_No.\tCity\tState\tZip_Code\tPhone\tN_U_Q' > data.txt

FILES=./permits/*

for file in $FILES
do
	echo "Processing $file..."
	awk -v var="$file" '{
		f[0]=substr(var,11,8);
		f[0]=substr(f[0],1,4)"-"substr(f[0],5,2)"-"substr(f[0],7,2);
		f[1]=substr($0,1,60);
		f[2]=substr($0,61,8);
		f[2]=substr(f[2],1,4)"-"substr(f[2],5,2)"-"substr(f[2],7,2);
		f[3]=substr($0,69,3);
		f[4]=substr($0,72,2);
		f[5]=substr($0,74,2);
		f[6]=substr($0,76,2);
		f[7]=substr($0,78,6);
		f[8]=substr($0,84,3);
		f[9]=substr($0,87,6);
		f[10]=substr($0,93,2);
		f[11]=substr($0,95,20);
		f[12]=substr($0,115,4);
		f[13]=substr($0,119,20);
		f[14]=substr($0,139,15);
		f[15]=substr($0,154,2);
		f[16]=substr($0,156,9);
		f[17]=substr($0,165,10);
		f[18]=substr($0,175,1);
	
		for (i = 0; i <= 18; i++) {
			gsub(/ *$/,"",f[i])
			gsub(/%/, "%%", f[i])
			gsub(/\"/, "", f[i])
			if (f[i] == "")
				f[i] = "NA"
			if (i != 18)
				printf "\""f[i]"\"\t"
			else
				printf "\""f[i]"\"\n"
		}
	}' $file >> data.txt
done