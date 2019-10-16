using System;
using System.Collections.Generic;

namespace ConsoleApp1
{
    public interface Expr
    {
        int Evaluate(IDictionary<string, int> env);
    }

    public class Cst : Expr
    {
        private readonly int _value;

        public Cst(int value)
        {
            _value = value;
        }

        public int Evaluate(IDictionary<string, int> env)
        {
            return _value;
        }
    }

    public class Var : Expr
    {
        private readonly string _key;

        public Var(string key)
        {
            _key = key;
        }

        public int Evaluate(IDictionary<string, int> env)
        {
            if(!env.TryGetValue(_key, out var value))
            {
                throw new NotImplementedException($"variable {_key} not found");
            }
            return value;
        }
    }

    public class Sum : Expr
    {
        private readonly Expr _expr1;
        private readonly Expr _expr2;

        public Sum(Expr expr1, Expr expr2)
        {
            _expr1 = expr1;
            _expr2 = expr2;
        }

        public int Evaluate(IDictionary<string, int> env)
        {
            return _expr1.Evaluate(env) + _expr2.Evaluate(env);
        }
    }

    public class Sub : Expr
    {
        private readonly Expr _expr1;
        private readonly Expr _expr2;

        public Sub(Expr expr1, Expr expr2)
        {
            _expr1 = expr1;
            _expr2 = expr2;
        }

        public int Evaluate(IDictionary<string, int> env)
        {
            return _expr1.Evaluate(env) - _expr2.Evaluate(env);
        }
    }

    class Program
    {
        static void Main(string[] args)
        {
            var env = new Dictionary<string, int> { ["a"] = 1, ["b"] = 2 };
            var expr = new Sub(new Sum(new Var("a"), new Var("b")), new Cst(2));
            Console.WriteLine(expr.Evaluate(env));
        }
    }
}
