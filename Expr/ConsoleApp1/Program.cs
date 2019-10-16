using System;
using System.Collections.Generic;
using System.Linq;

namespace ConsoleApp1
{
    public interface IExpr
    {
        int Evaluate(IDictionary<string, int> env);
    }

    public class Cst : IExpr
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

    public class Var : IExpr
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

    public class Sum : IExpr
    {
        private readonly IExpr _expr1;
        private readonly IExpr _expr2;

        public Sum(IExpr expr1, IExpr expr2)
        {
            _expr1 = expr1;
            _expr2 = expr2;
        }

        public int Evaluate(IDictionary<string, int> env)
        {
            return _expr1.Evaluate(env) + _expr2.Evaluate(env);
        }
    }

    public class Sub : IExpr
    {
        private readonly IExpr _expr1;
        private readonly IExpr _expr2;

        public Sub(IExpr expr1, IExpr expr2)
        {
            _expr1 = expr1;
            _expr2 = expr2;
        }

        public int Evaluate(IDictionary<string, int> env)
        {
            return _expr1.Evaluate(env) - _expr2.Evaluate(env);
        }
    }

    public class Let : IExpr
    {
        private readonly string _key;
        private readonly IExpr _value;
        private readonly IExpr _expr;

        public Let(string key, IExpr value, IExpr expr)
        {
            _key = key;
            _value = value;
            _expr = expr;
        }

        public int Evaluate(IDictionary<string, int> env)
        {
            var v1 = _value.Evaluate(env);
            var env1 = new Dictionary<string, int>(env) {[_key] = v1};
            return _expr.Evaluate(env1);
        }
    }

    class Program
    {
        static void Main(string[] args)
        {
            var env = new Dictionary<string, int> { ["a"] = 1 };
            var expr = new Sub(
                new Sum(new Cst(2), new Let("a", new Cst(2), new Var("a"))), 
                new Var("a"));
            Console.WriteLine(expr.Evaluate(env));
        }
    }
}
