using System;

namespace rebase2
{
	public class Utils
	{
		public static void Retry<T, Err>(Func<T> func) where Err: Exception
		{
			while(1) {
				try {
					return func();
				} catch (Err err) {
					Console.Error.WriteLine("Error: {0}\nRetry (Y/n)?", err.Message);
					string input = Console.In.ReadLine();
					if(!(input != null && (input.StartsWith("y") || input.StartsWith("Y")))) {
						throw;
					}
				}
			}
		}
	}
}

