﻿using System;
using System.Collections.Generic;
using NUnit.Framework;
using recommender;
using System.Linq;

namespace Test
{
	[TestFixture]
	public class UnitTest1
	{
		private List<Resource> resources = null;
		private VectorSpace space = null;
		[SetUp]
		public void Setup()
		{
			var serializer = Newtonsoft.Json.JsonSerializer.Create();
			using (var treader = new System.IO.StringReader(Resource1.testData1)) 
			{
				using (var reader = new Newtonsoft.Json.JsonTextReader(treader)) 
				{
					resources = serializer.Deserialize<List<Resource>>(reader);
				}
			}
			space = new VectorSpace(resources);
		}

		[TestCase(TestName = "Should recommend")]
		public void TestMethod1()
		{
			var resultSet = space.ComputeRecommendations(1);
			var expected = resultSet.First(r => r.Uri == "http://google.com/").Result.First();
			Assert.AreEqual("http://yahoo.com/", expected.Item2);
		}
		[TestCase(TestName = "Should recommend 2")]
		public void TestMethod2()
		{
			var resultSet = space.ComputeRecommendations(3);
			var expected = resultSet.First(r => r.Uri == "http://www.jshint.com/docs/").Result.First();
			Assert.AreEqual("http://seravo.fi/2013/javascript-the-winning-style", expected.Item2);
		}
	}
}
