
import os
import re
from bs4 import BeautifulSoup



def clean_word(word):
	word = word.lower()
	if word == "'s":
		return ""
	while word and not word[0].isalnum():
		word = word[1:]
	while word and not word[-1].isalnum():
		word = word[0:-1]
	if word.endswith("'s"):
		word = word[0:-2]
	return word

fileList = os.listdir(r"C:\Users\natas\Downloads\InspiredFics")
allTexts = {}
for fileName in fileList:
	if fileName != '.idea':
		fileLocation = r"C:\Users\natas\Downloads\InspiredFics\\" + fileName
		with open(fileLocation, encoding="utf8") as f:
			text = f.read()
			allTexts[fileName] = text



import csv
with open('InspiredFics.csv', 'w', newline='', encoding="utf-8") as file:
	writer = csv.writer(file)
	writer.writerow(["title", "author", "published", "completed", "words", "inspired", "text"])
	for fanfic in allTexts:
		soup = BeautifulSoup(allTexts[fanfic], 'html.parser')

		title = soup.b.string
		author = soup.find(rel='author').string
		alldd = soup.find_all('dd')
		allddList = []
		for points in alldd:
			elem = str(points.text)
			allddList.append(elem)
		stats = [x for x in allddList if re.search("Published", x)]
		stats = stats[0]
		dateIndex = stats.index('Published')
		Publisheddate = ''
		for i in range(dateIndex + 11, dateIndex + 21):
			Publisheddate = Publisheddate + stats[i]
		published = Publisheddate
		if "Completed" in stats:
			CompleteddateIndex = stats.index('Completed')
			Completeddate = ''
			for i in range(CompleteddateIndex + 11, CompleteddateIndex + 21):
				Completeddate = Completeddate + stats[i]
			completed = Completeddate
		else:
			completed = Publisheddate
		wordCount = ''
		wordIndex = stats.index("Words")
		wordEnd = 0
		for i in range(wordIndex + 7, wordIndex + 30):
			if stats[i].isdigit():
				wordEnd += 1
			else:
				break
		for i in range(wordIndex + 7, wordIndex + 7 + wordEnd):
			wordCount = wordCount + stats[i]
		words = wordCount

		afterword = soup.find(id='afterword')
		afterwordChildren = afterword.findChildren('a')
		afterwordList = []
		for elem in afterwordChildren:
			afterwordList.append(elem['href'])
		worksInspired = [x for x in afterwordList if re.search("/works", x) and not re.search("/comments", x)]
		inspired = worksInspired

		all_chapters = soup.find_all(id="chapters")
		segments = all_chapters[0].find_all("p")
		story_text = []
		for paragraph in segments:
			story_text.append(paragraph.get_text())
		story_text = " ".join(story_text)
		story_text = story_text.split()
		story_text = [clean_word(word) for word in story_text]
		story_text = " ".join(story_text)
		text = story_text


		writer.writerow([title, author, published, completed, words, inspired, text])



